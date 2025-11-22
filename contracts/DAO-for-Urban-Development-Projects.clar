(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-PROPOSAL-EXPIRED (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))
(define-constant PROPOSAL-DURATION u1440)
(define-constant MIN-PROPOSAL-AMOUNT u1000000)
(define-constant VOTING_POWER_MULTIPLIER u100)

(define-data-var dao-owner principal tx-sender)
(define-data-var proposal-count uint u0)
(define-data-var total-funds uint u0)

(define-map proposals
    uint 
    {
        creator: principal,
        title: (string-ascii 50),
        description: (string-ascii 500),
        amount: uint,
        votes: uint,
        status: (string-ascii 20),
        deadline: uint,
        executed: bool
    }
)

(define-map member-stakes principal uint)
(define-map votes {proposal-id: uint, voter: principal} bool)

(define-public (initialize-dao)
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (ok true)))

(define-public (create-proposal (title (string-ascii 50)) (description (string-ascii 500)) (amount uint))
    (let ((proposal-id (+ (var-get proposal-count) u1)))
        (try! (check-dao-active))
        (asserts! (>= amount MIN-PROPOSAL-AMOUNT) ERR-INVALID-AMOUNT)
        (map-set proposals proposal-id
            {
                creator: tx-sender,
                title: title,
                description: description,
                amount: amount,
                votes: u0,
                status: "active",
                deadline: (+ burn-block-height PROPOSAL-DURATION),
                executed: false
            }
        )
        (var-set proposal-count proposal-id)
        (ok proposal-id)))

(define-public (stake-tokens (amount uint))
    (let ((current-stake (default-to u0 (map-get? member-stakes tx-sender))))
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set member-stakes tx-sender (+ current-stake amount))
        (var-set total-funds (+ (var-get total-funds) amount))
        (ok true)))

(define-public (vote-on-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (voter-stake (default-to u0 (map-get? member-stakes tx-sender)))
    )
        (asserts! (not (default-to false (map-get? votes {proposal-id: proposal-id, voter: tx-sender}))) ERR-ALREADY-VOTED)
        (asserts! (< burn-block-height (get deadline proposal)) ERR-PROPOSAL-EXPIRED)
        (map-set proposals proposal-id 
            (merge proposal {votes: (+ (get votes proposal) (* voter-stake VOTING_POWER_MULTIPLIER))})
        )
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} true)
        (ok true)))

(define-public (execute-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND)))
        (asserts! (is-eq (get status proposal) "active") ERR-PROPOSAL-NOT-FOUND)
        (asserts! (>= (var-get total-funds) (get amount proposal)) ERR-INSUFFICIENT-FUNDS)
        (map-set proposals proposal-id
            (merge proposal {
                status: "executed",
                executed: true
            })
        )
        (var-set total-funds (- (var-get total-funds) (get amount proposal)))
        (ok true)))

(define-read-only (get-proposal (proposal-id uint))
    (ok (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND)))

(define-read-only (get-member-stake (member principal))
    (ok (default-to u0 (map-get? member-stakes member))))

(define-read-only (get-total-funds)
    (ok (var-get total-funds)))

(define-constant QUORUM-PERCENTAGE u20)
(define-constant APPROVAL-THRESHOLD u60)

(define-data-var total-staked-tokens uint u0)

(define-map proposal-votes
    uint
    {
        yes-votes: uint,
        no-votes: uint,
        total-voters: uint
    }
)

(define-public (stake-tokens-enhanced (amount uint))
    (let ((current-stake (default-to u0 (map-get? member-stakes tx-sender))))
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set member-stakes tx-sender (+ current-stake amount))
        (var-set total-funds (+ (var-get total-funds) amount))
        (var-set total-staked-tokens (+ (var-get total-staked-tokens) amount))
        (ok true)))

(define-public (vote-on-proposal-enhanced (proposal-id uint) (vote-yes bool))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (voter-stake (default-to u0 (map-get? member-stakes tx-sender)))
        (current-votes (default-to {yes-votes: u0, no-votes: u0, total-voters: u0} 
                                  (map-get? proposal-votes proposal-id)))
    )
        (try! (check-dao-active))
        (asserts! (not (default-to false (map-get? votes {proposal-id: proposal-id, voter: tx-sender}))) ERR-ALREADY-VOTED)
        (asserts! (< burn-block-height (get deadline proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (> voter-stake u0) ERR-NOT-AUTHORIZED)
        
        (let ((voting-power (* voter-stake VOTING_POWER_MULTIPLIER)))
            (if vote-yes
                (map-set proposal-votes proposal-id
                    {
                        yes-votes: (+ (get yes-votes current-votes) voting-power),
                        no-votes: (get no-votes current-votes),
                        total-voters: (+ (get total-voters current-votes) u1)
                    })
                (map-set proposal-votes proposal-id
                    {
                        yes-votes: (get yes-votes current-votes),
                        no-votes: (+ (get no-votes current-votes) voting-power),
                        total-voters: (+ (get total-voters current-votes) u1)
                    })
            )
        )
        
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} true)
        (ok true)))

(define-public (execute-proposal-enhanced (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (vote-data (default-to {yes-votes: u0, no-votes: u0, total-voters: u0} 
                               (map-get? proposal-votes proposal-id)))
        (total-votes (+ (get yes-votes vote-data) (get no-votes vote-data)))
        (total-possible-votes (* (var-get total-staked-tokens) VOTING_POWER_MULTIPLIER))
    )
        (asserts! (is-eq (get status proposal) "active") ERR-PROPOSAL-NOT-FOUND)
        (asserts! (>= burn-block-height (get deadline proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (>= (var-get total-funds) (get amount proposal)) ERR-INSUFFICIENT-FUNDS)
        
        (let (
            (participation-rate (/ (* total-votes u100) total-possible-votes))
            (approval-rate (if (> total-votes u0) (/ (* (get yes-votes vote-data) u100) total-votes) u0))
        )
            (asserts! (>= participation-rate QUORUM-PERCENTAGE) ERR-NOT-AUTHORIZED)
            (asserts! (>= approval-rate APPROVAL-THRESHOLD) ERR-NOT-AUTHORIZED)
            
            (map-set proposals proposal-id
                (merge proposal {
                    status: "executed",
                    executed: true
                })
            )
            (var-set total-funds (- (var-get total-funds) (get amount proposal)))
            (ok true))))

(define-read-only (get-proposal-votes (proposal-id uint))
    (ok (map-get? proposal-votes proposal-id)))

(define-read-only (check-proposal-status (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (vote-data (default-to {yes-votes: u0, no-votes: u0, total-voters: u0} 
                               (map-get? proposal-votes proposal-id)))
        (total-votes (+ (get yes-votes vote-data) (get no-votes vote-data)))
        (total-possible-votes (* (var-get total-staked-tokens) VOTING_POWER_MULTIPLIER))
    )
        (if (> total-votes u0)
            (ok {
                participation-rate: (/ (* total-votes u100) total-possible-votes),
                approval-rate: (/ (* (get yes-votes vote-data) u100) total-votes),
                meets-quorum: (>= (/ (* total-votes u100) total-possible-votes) QUORUM-PERCENTAGE),
                meets-threshold: (>= (/ (* (get yes-votes vote-data) u100) total-votes) APPROVAL-THRESHOLD)
            })
            (ok {
                participation-rate: u0,
                approval-rate: u0,
                meets-quorum: false,
                meets-threshold: false
            }))))

            (define-constant ERR-MILESTONE-NOT-FOUND (err u106))
(define-constant ERR-MILESTONE-ALREADY-COMPLETED (err u107))
(define-constant ERR-PREVIOUS-MILESTONE-INCOMPLETE (err u108))
(define-constant MAX-MILESTONES u10)

(define-data-var milestone-count uint u0)

(define-map project-milestones
    uint
    {
        proposal-id: uint,
        milestone-number: uint,
        description: (string-ascii 200),
        amount: uint,
        completed: bool,
        approval-votes: uint,
        rejection-votes: uint,
        deadline: uint
    }
)

(define-map proposal-milestone-count uint uint)

(define-map milestone-votes {milestone-id: uint, voter: principal} bool)

(define-public (create-milestone-proposal (title (string-ascii 50)) (description (string-ascii 500)) 
                                        (milestone-descriptions (list 10 (string-ascii 200)))
                                        (milestone-amounts (list 10 uint)))
    (let (
        (proposal-id (+ (var-get proposal-count) u1))
        (total-amount (fold + milestone-amounts u0))
        (milestone-count-val (len milestone-descriptions))
    )
        (asserts! (>= total-amount MIN-PROPOSAL-AMOUNT) ERR-INVALID-AMOUNT)
        (asserts! (is-eq (len milestone-descriptions) (len milestone-amounts)) ERR-INVALID-AMOUNT)
        (asserts! (<= milestone-count-val MAX-MILESTONES) ERR-INVALID-AMOUNT)
        
        (map-set proposals proposal-id
            {
                creator: tx-sender,
                title: title,
                description: description,
                amount: total-amount,
                votes: u0,
                status: "active",
                deadline: (+ burn-block-height PROPOSAL-DURATION),
                executed: false
            }
        )
        
        (var-set proposal-count proposal-id)
        (map-set proposal-milestone-count proposal-id milestone-count-val)
        
        (let ((milestone-creation-result 
               (fold create-milestone-helper-indexed 
                     (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
                     {proposal-id: proposal-id, current-milestone: u0, success: true, descriptions: milestone-descriptions, amounts: milestone-amounts})))
            (if (get success milestone-creation-result)
                (ok proposal-id)
                ERR-INVALID-AMOUNT))))

(define-private (create-milestone-helper-indexed (index uint)
                                               (acc {proposal-id: uint, current-milestone: uint, success: bool, descriptions: (list 10 (string-ascii 200)), amounts: (list 10 uint)}))
    (if (and (get success acc) (< index (len (get descriptions acc))))
        (let ((milestone-id (+ (var-get milestone-count) u1))
              (milestone-num (+ (get current-milestone acc) u1))
              (description (unwrap-panic (element-at? (get descriptions acc) index)))
              (amount (unwrap-panic (element-at? (get amounts acc) index))))
            (map-set project-milestones milestone-id
                {
                    proposal-id: (get proposal-id acc),
                    milestone-number: milestone-num,
                    description: description,
                    amount: amount,
                    completed: false,
                    approval-votes: u0,
                    rejection-votes: u0,
                    deadline: (+ burn-block-height (* PROPOSAL-DURATION u2))
                }
            )
            (var-set milestone-count milestone-id)
            {
                proposal-id: (get proposal-id acc),
                current-milestone: milestone-num,
                success: true,
                descriptions: (get descriptions acc),
                amounts: (get amounts acc)
            })
        acc))

(define-public (vote-on-milestone (milestone-id uint) (approve bool))
    (let (
        (milestone (unwrap! (map-get? project-milestones milestone-id) ERR-MILESTONE-NOT-FOUND))
        (voter-stake (default-to u0 (map-get? member-stakes tx-sender)))
        (voting-power (* voter-stake VOTING_POWER_MULTIPLIER))
    )
        (asserts! (not (default-to false (map-get? milestone-votes {milestone-id: milestone-id, voter: tx-sender}))) ERR-ALREADY-VOTED)
        (asserts! (< burn-block-height (get deadline milestone)) ERR-PROPOSAL-EXPIRED)
        (asserts! (not (get completed milestone)) ERR-MILESTONE-ALREADY-COMPLETED)
        (asserts! (> voter-stake u0) ERR-NOT-AUTHORIZED)
        
        (if (> (get milestone-number milestone) u1)
            (asserts! (is-previous-milestone-completed (get proposal-id milestone) (- (get milestone-number milestone) u1)) ERR-PREVIOUS-MILESTONE-INCOMPLETE)
            true)
        
        (if approve
            (map-set project-milestones milestone-id
                (merge milestone {approval-votes: (+ (get approval-votes milestone) voting-power)}))
            (map-set project-milestones milestone-id
                (merge milestone {rejection-votes: (+ (get rejection-votes milestone) voting-power)})))
        
        (map-set milestone-votes {milestone-id: milestone-id, voter: tx-sender} true)
        (ok true)))

(define-public (complete-milestone (milestone-id uint))
    (let (
        (milestone (unwrap! (map-get? project-milestones milestone-id) ERR-MILESTONE-NOT-FOUND))
        (total-votes (+ (get approval-votes milestone) (get rejection-votes milestone)))
        (total-possible-votes (* (var-get total-staked-tokens) VOTING_POWER_MULTIPLIER))
    )
        (asserts! (not (get completed milestone)) ERR-MILESTONE-ALREADY-COMPLETED)
        (asserts! (>= burn-block-height (get deadline milestone)) ERR-PROPOSAL-EXPIRED)
        (asserts! (>= (var-get total-funds) (get amount milestone)) ERR-INSUFFICIENT-FUNDS)
        
        (let (
            (participation-rate (/ (* total-votes u100) total-possible-votes))
            (approval-rate (if (> total-votes u0) (/ (* (get approval-votes milestone) u100) total-votes) u0))
        )
            (asserts! (>= participation-rate QUORUM-PERCENTAGE) ERR-NOT-AUTHORIZED)
            (asserts! (>= approval-rate APPROVAL-THRESHOLD) ERR-NOT-AUTHORIZED)
            
            (map-set project-milestones milestone-id
                (merge milestone {completed: true}))
            (var-set total-funds (- (var-get total-funds) (get amount milestone)))
            (ok true))))

(define-private (is-previous-milestone-completed (proposal-id uint) (milestone-number uint))
    (let ((milestone-id (get-milestone-id-by-number proposal-id milestone-number)))
        (match milestone-id
            some-id (let ((milestone (map-get? project-milestones some-id)))
                       (match milestone
                           some-milestone (get completed some-milestone)
                           false))
            false)))

(define-private (get-milestone-id-by-number (proposal-id uint) (milestone-number uint))
    (let ((search-result (fold find-milestone-helper 
                              (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)
                              {target-proposal: proposal-id, target-milestone: milestone-number, found-id: none})))
        (get found-id search-result)))

(define-private (find-milestone-helper (milestone-id uint) 
                                     (acc {target-proposal: uint, target-milestone: uint, found-id: (optional uint)}))
    (if (is-none (get found-id acc))
        (let ((milestone (map-get? project-milestones milestone-id)))
            (match milestone
                some-milestone (if (and (is-eq (get proposal-id some-milestone) (get target-proposal acc))
                                       (is-eq (get milestone-number some-milestone) (get target-milestone acc)))
                                  {target-proposal: (get target-proposal acc), 
                                   target-milestone: (get target-milestone acc), 
                                   found-id: (some milestone-id)}
                                  acc)
                acc))
        acc))

(define-read-only (get-milestone (milestone-id uint))
    (ok (map-get? project-milestones milestone-id)))

(define-read-only (get-proposal-milestones (proposal-id uint))
    (ok (map-get? proposal-milestone-count proposal-id)))

(define-private (count-milestone-helper (milestone-id uint) 
                                       (acc {target-proposal: uint, completed-count: uint}))
    (let ((milestone (map-get? project-milestones milestone-id)))
        (match milestone
            some-milestone (if (and (is-eq (get proposal-id some-milestone) (get target-proposal acc))
                                   (get completed some-milestone))
                              {target-proposal: (get target-proposal acc), 
                               completed-count: (+ (get completed-count acc) u1)}
                              acc)
            acc)))

(define-private (count-completed-milestones (proposal-id uint))
    (let ((search-result (fold count-milestone-helper 
                              (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20)
                              {target-proposal: proposal-id, completed-count: u0})))
        (get completed-count search-result)))

(define-read-only (get-milestone-progress (proposal-id uint))
    (let ((total-milestones (default-to u0 (map-get? proposal-milestone-count proposal-id))))
        (ok {
            total-milestones: total-milestones,
            completed-milestones: (count-completed-milestones proposal-id)
        })))

(define-constant ERR-DAO-PAUSED (err u109))
(define-constant ERR-EMERGENCY-TIMEOUT (err u110))
(define-constant ERR-INSUFFICIENT-EMERGENCY-VOTES (err u111))
(define-constant EMERGENCY-PAUSE-DURATION u2016)
(define-constant EMERGENCY-VOTE_THRESHOLD u75)

(define-data-var dao-paused bool false)
(define-data-var pause-expiry uint u0)
(define-data-var emergency-proposal-count uint u0)

(define-map emergency-proposals
    uint
    {
        creator: principal,
        action: (string-ascii 20),
        reason: (string-ascii 300),
        votes: uint,
        executed: bool,
        deadline: uint
    }
)

(define-map emergency-votes {proposal-id: uint, voter: principal} bool)

(define-private (check-dao-active)
    (if (var-get dao-paused)
        (if (<= burn-block-height (var-get pause-expiry))
            ERR-DAO-PAUSED
            (begin
                (var-set dao-paused false)
                (var-set pause-expiry u0)
                (ok true)))
        (ok true)))

(define-public (create-emergency-proposal (action (string-ascii 20)) (reason (string-ascii 300)))
    (let ((emergency-id (+ (var-get emergency-proposal-count) u1)))
        (asserts! (or (is-eq action "pause") (is-eq action "unpause")) ERR-INVALID-AMOUNT)
        (asserts! (> (default-to u0 (map-get? member-stakes tx-sender)) u0) ERR-NOT-AUTHORIZED)
        
        (map-set emergency-proposals emergency-id
            {
                creator: tx-sender,
                action: action,
                reason: reason,
                votes: u0,
                executed: false,
                deadline: (+ burn-block-height u144)
            }
        )
        (var-set emergency-proposal-count emergency-id)
        (ok emergency-id)))

(define-public (vote-emergency-proposal (emergency-id uint))
    (let (
        (emergency (unwrap! (map-get? emergency-proposals emergency-id) ERR-PROPOSAL-NOT-FOUND))
        (voter-stake (default-to u0 (map-get? member-stakes tx-sender)))
        (voting-power (* voter-stake VOTING_POWER_MULTIPLIER))
    )
        (asserts! (not (default-to false (map-get? emergency-votes {proposal-id: emergency-id, voter: tx-sender}))) ERR-ALREADY-VOTED)
        (asserts! (< burn-block-height (get deadline emergency)) ERR-PROPOSAL-EXPIRED)
        (asserts! (not (get executed emergency)) ERR-PROPOSAL-NOT-FOUND)
        (asserts! (> voter-stake u0) ERR-NOT-AUTHORIZED)
        
        (map-set emergency-proposals emergency-id
            (merge emergency {votes: (+ (get votes emergency) voting-power)})
        )
        (map-set emergency-votes {proposal-id: emergency-id, voter: tx-sender} true)
        (ok true)))

(define-public (execute-emergency-proposal (emergency-id uint))
    (let (
        (emergency (unwrap! (map-get? emergency-proposals emergency-id) ERR-PROPOSAL-NOT-FOUND))
        (total-possible-votes (* (var-get total-staked-tokens) VOTING_POWER_MULTIPLIER))
        (vote-percentage (if (> total-possible-votes u0) (/ (* (get votes emergency) u100) total-possible-votes) u0))
    )
        (asserts! (not (get executed emergency)) ERR-PROPOSAL-NOT-FOUND)
        (asserts! (>= burn-block-height (get deadline emergency)) ERR-PROPOSAL-EXPIRED)
        (asserts! (>= vote-percentage EMERGENCY-VOTE_THRESHOLD) ERR-INSUFFICIENT-EMERGENCY-VOTES)
        
        (if (is-eq (get action emergency) "pause")
            (begin
                (var-set dao-paused true)
                (var-set pause-expiry (+ burn-block-height EMERGENCY-PAUSE-DURATION))
                (map-set emergency-proposals emergency-id (merge emergency {executed: true}))
                (ok true))
            (if (is-eq (get action emergency) "unpause")
                (begin
                    (var-set dao-paused false)
                    (var-set pause-expiry u0)
                    (map-set emergency-proposals emergency-id (merge emergency {executed: true}))
                    (ok true))
                ERR-INVALID-AMOUNT))))

(define-read-only (get-emergency-status)
    (ok {
        is-paused: (var-get dao-paused),
        pause-expiry: (var-get pause-expiry),
        blocks-remaining: (if (var-get dao-paused) (- (var-get pause-expiry) burn-block-height) u0)
    }))

(define-read-only (get-emergency-proposal (emergency-id uint))
    (ok (map-get? emergency-proposals emergency-id)))

(define-constant ERR-INSUFFICIENT-REPUTATION (err u112))
(define-constant MIN-REPUTATION-FOR-PROPOSAL u50)
(define-constant REPUTATION-VOTE-BONUS u5)
(define-constant REPUTATION-PROPOSAL_SUCCESS_BONUS u20)
(define-constant REPUTATION-PROPOSAL_FAILURE_PENALTY u10)
(define-constant MAX-REPUTATION u1000)
(define-constant INITIAL-REPUTATION u100)

(define-data-var reputation-updates-count uint u0)

(define-map member-reputation principal {
    score: uint,
    proposals-created: uint,
    successful-proposals: uint,
    votes-cast: uint,
    correct-votes: uint,
    last-activity: uint
})

(define-map proposal-outcome-tracking uint {
    final-status: (string-ascii 20),
    supporters: (list 50 principal),
    opposers: (list 50 principal)
})

(define-public (initialize-member-reputation)
    (let ((current-rep (map-get? member-reputation tx-sender)))
        (if (is-none current-rep)
            (begin
                (map-set member-reputation tx-sender {
                    score: INITIAL-REPUTATION,
                    proposals-created: u0,
                    successful-proposals: u0,
                    votes-cast: u0,
                    correct-votes: u0,
                    last-activity: burn-block-height
                })
                (ok true))
            (ok true))))

(define-private (min-value (a uint) (b uint))
    (if (< a b) a b))

(define-public (update-reputation-for-vote (member principal) (proposal-id uint) (vote-type bool))
    (let (
        (current-rep (default-to {
            score: INITIAL-REPUTATION,
            proposals-created: u0,
            successful-proposals: u0,
            votes-cast: u0,
            correct-votes: u0,
            last-activity: u0
        } (map-get? member-reputation member)))
    )
        (map-set member-reputation member {
            score: (min-value (+ (get score current-rep) REPUTATION-VOTE-BONUS) MAX-REPUTATION),
            proposals-created: (get proposals-created current-rep),
            successful-proposals: (get successful-proposals current-rep),
            votes-cast: (+ (get votes-cast current-rep) u1),
            correct-votes: (get correct-votes current-rep),
            last-activity: burn-block-height
        })
        (ok true)))

(define-public (create-proposal-with-reputation (title (string-ascii 50)) (description (string-ascii 500)) (amount uint))
    (let (
        (member-rep (default-to {
            score: INITIAL-REPUTATION,
            proposals-created: u0,
            successful-proposals: u0,
            votes-cast: u0,
            correct-votes: u0,
            last-activity: u0
        } (map-get? member-reputation tx-sender)))
        (proposal-id (+ (var-get proposal-count) u1))
    )
        (try! (check-dao-active))
        (asserts! (>= (get score member-rep) MIN-REPUTATION-FOR-PROPOSAL) ERR-INSUFFICIENT-REPUTATION)
        (asserts! (>= amount MIN-PROPOSAL-AMOUNT) ERR-INVALID-AMOUNT)
        
        (map-set proposals proposal-id {
            creator: tx-sender,
            title: title,
            description: description,
            amount: amount,
            votes: u0,
            status: "active",
            deadline: (+ burn-block-height PROPOSAL-DURATION),
            executed: false
        })
        
        (map-set member-reputation tx-sender {
            score: (get score member-rep),
            proposals-created: (+ (get proposals-created member-rep) u1),
            successful-proposals: (get successful-proposals member-rep),
            votes-cast: (get votes-cast member-rep),
            correct-votes: (get correct-votes member-rep),
            last-activity: burn-block-height
        })
        
        (var-set proposal-count proposal-id)
        (ok proposal-id)))

(define-public (finalize-proposal-outcome (proposal-id uint) (success bool))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (creator (get creator proposal))
        (creator-rep (default-to {
            score: INITIAL-REPUTATION,
            proposals-created: u0,
            successful-proposals: u0,
            votes-cast: u0,
            correct-votes: u0,
            last-activity: u0
        } (map-get? member-reputation creator)))
    )
        (asserts! (or (is-eq (get status proposal) "executed") 
                     (>= burn-block-height (get deadline proposal))) ERR-PROPOSAL-NOT-FOUND)
        
        (if success
            (map-set member-reputation creator {
                score: (min-value (+ (get score creator-rep) REPUTATION-PROPOSAL_SUCCESS_BONUS) MAX-REPUTATION),
                proposals-created: (get proposals-created creator-rep),
                successful-proposals: (+ (get successful-proposals creator-rep) u1),
                votes-cast: (get votes-cast creator-rep),
                correct-votes: (get correct-votes creator-rep),
                last-activity: burn-block-height
            })
            (map-set member-reputation creator {
                score: (if (> (get score creator-rep) REPUTATION-PROPOSAL_FAILURE_PENALTY)
                          (- (get score creator-rep) REPUTATION-PROPOSAL_FAILURE_PENALTY)
                          u0),
                proposals-created: (get proposals-created creator-rep),
                successful-proposals: (get successful-proposals creator-rep),
                votes-cast: (get votes-cast creator-rep),
                correct-votes: (get correct-votes creator-rep),
                last-activity: burn-block-height
            }))
        
        (map-set proposal-outcome-tracking proposal-id {
            final-status: (if success "successful" "failed"),
            supporters: (list),
            opposers: (list)
        })
        
        (ok true)))

(define-public (get-weighted-voting-power (member principal))
    (let (
        (base-stake (default-to u0 (map-get? member-stakes member)))
        (reputation-data (map-get? member-reputation member))
    )
        (match reputation-data
            some-rep (let (
                (reputation-multiplier (/ (get score some-rep) u10))
                (base-power (* base-stake VOTING_POWER_MULTIPLIER))
            )
                (ok (+ base-power reputation-multiplier)))
            (ok (* base-stake VOTING_POWER_MULTIPLIER)))))

(define-public (vote-with-reputation (proposal-id uint) (vote-yes bool))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (voting-power-result (unwrap! (get-weighted-voting-power tx-sender) ERR-NOT-AUTHORIZED))
        (current-votes (default-to {yes-votes: u0, no-votes: u0, total-voters: u0} 
                                  (map-get? proposal-votes proposal-id)))
    )
        (try! (check-dao-active))
        (unwrap! (initialize-member-reputation) ERR-NOT-AUTHORIZED)
        (asserts! (not (default-to false (map-get? votes {proposal-id: proposal-id, voter: tx-sender}))) ERR-ALREADY-VOTED)
        (asserts! (< burn-block-height (get deadline proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (> (default-to u0 (map-get? member-stakes tx-sender)) u0) ERR-NOT-AUTHORIZED)
        
        (if vote-yes
            (map-set proposal-votes proposal-id {
                yes-votes: (+ (get yes-votes current-votes) voting-power-result),
                no-votes: (get no-votes current-votes),
                total-voters: (+ (get total-voters current-votes) u1)
            })
            (map-set proposal-votes proposal-id {
                yes-votes: (get yes-votes current-votes),
                no-votes: (+ (get no-votes current-votes) voting-power-result),
                total-voters: (+ (get total-voters current-votes) u1)
            }))
        
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} true)
        (unwrap! (update-reputation-for-vote tx-sender proposal-id vote-yes) ERR-NOT-AUTHORIZED)
        (ok true)))

(define-read-only (get-member-reputation (member principal))
    (ok (map-get? member-reputation member)))

(define-read-only (calculate-member-governance-score (member principal))
    (let (
        (rep-data (map-get? member-reputation member))
        (stake (default-to u0 (map-get? member-stakes member)))
    )
        (match rep-data
            some-rep (let (
                (participation-rate (if (> (get votes-cast some-rep) u0)
                                      (/ (* (get correct-votes some-rep) u100) (get votes-cast some-rep))
                                      u0))
                (proposal-success-rate (if (> (get proposals-created some-rep) u0)
                                         (/ (* (get successful-proposals some-rep) u100) (get proposals-created some-rep))
                                         u0))
                (activity-score (get score some-rep))
                (governance-score (/ (+ participation-rate proposal-success-rate activity-score) u3))
            )
                (ok {
                    reputation-score: activity-score,
                    participation-rate: participation-rate,
                    proposal-success-rate: proposal-success-rate,
                    governance-score: governance-score,
                    stake-amount: stake,
                    total-influence: (+ governance-score (/ stake u1000))
                }))
            (ok {
                reputation-score: u0,
                participation-rate: u0,
                proposal-success-rate: u0,
                governance-score: u0,
                stake-amount: stake,
                total-influence: (/ stake u1000)
            }))))

(define-read-only (get-top-contributors)
    (ok "Feature available - implement ranking algorithm based on governance scores"))

;; PROPOSAL DEPOSIT ESCROW SYSTEM
;; Constants for deposit system
(define-constant ERR-DEPOSIT-NOT-FOUND (err u113))
(define-constant ERR-DEPOSIT-ALREADY-PROCESSED (err u114))
(define-constant PROPOSAL-DEPOSIT-AMOUNT u500000) ;; 0.5 STX deposit required

;; Map to track proposal deposits
(define-map proposal-deposits
    uint ;; proposal-id
    {
        proposer: principal,
        deposit-amount: uint,
        refunded: bool,
        forfeited: bool,
        processed-at: (optional uint)
    }
)

;; Public function: Create proposal with deposit escrow
(define-public (create-proposal-with-deposit 
    (title (string-ascii 50)) 
    (description (string-ascii 500)) 
    (amount uint)
)
    (let (
        (proposal-id (+ (var-get proposal-count) u1))
        (member-rep (default-to {
            score: INITIAL-REPUTATION,
            proposals-created: u0,
            successful-proposals: u0,
            votes-cast: u0,
            correct-votes: u0,
            last-activity: u0
        } (map-get? member-reputation tx-sender)))
    )
        (try! (check-dao-active))
        (asserts! (>= (get score member-rep) MIN-REPUTATION-FOR-PROPOSAL) ERR-INSUFFICIENT-REPUTATION)
        (asserts! (>= amount MIN-PROPOSAL-AMOUNT) ERR-INVALID-AMOUNT)
        
        ;; Transfer deposit to contract escrow
        (try! (stx-transfer? PROPOSAL-DEPOSIT-AMOUNT tx-sender (as-contract tx-sender)))
        
        ;; Create the proposal
        (map-set proposals proposal-id {
            creator: tx-sender,
            title: title,
            description: description,
            amount: amount,
            votes: u0,
            status: "active",
            deadline: (+ burn-block-height PROPOSAL-DURATION),
            executed: false
        })
        
        ;; Store deposit information
        (map-set proposal-deposits proposal-id {
            proposer: tx-sender,
            deposit-amount: PROPOSAL-DEPOSIT-AMOUNT,
            refunded: false,
            forfeited: false,
            processed-at: none
        })
        
        ;; Update reputation tracking
        (map-set member-reputation tx-sender {
            score: (get score member-rep),
            proposals-created: (+ (get proposals-created member-rep) u1),
            successful-proposals: (get successful-proposals member-rep),
            votes-cast: (get votes-cast member-rep),
            correct-votes: (get correct-votes member-rep),
            last-activity: burn-block-height
        })
        
        (var-set proposal-count proposal-id)
        (ok proposal-id)
    )
)

;; Public function: Process proposal deposit after voting period
(define-public (finalize-proposal-with-deposit (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (deposit (unwrap! (map-get? proposal-deposits proposal-id) ERR-DEPOSIT-NOT-FOUND))
        (vote-data (default-to {yes-votes: u0, no-votes: u0, total-voters: u0} 
                               (map-get? proposal-votes proposal-id)))
        (total-votes (+ (get yes-votes vote-data) (get no-votes vote-data)))
        (total-possible-votes (* (var-get total-staked-tokens) VOTING_POWER_MULTIPLIER))
    )
        ;; Check that voting period has ended
        (asserts! (>= burn-block-height (get deadline proposal)) ERR-PROPOSAL-EXPIRED)
        ;; Check deposit hasn't been processed yet
        (asserts! (not (get refunded deposit)) ERR-DEPOSIT-ALREADY-PROCESSED)
        (asserts! (not (get forfeited deposit)) ERR-DEPOSIT-ALREADY-PROCESSED)
        
        (let (
            (participation-rate (if (> total-possible-votes u0) 
                                  (/ (* total-votes u100) total-possible-votes) 
                                  u0))
            (approval-rate (if (> total-votes u0) 
                             (/ (* (get yes-votes vote-data) u100) total-votes) 
                             u0))
            (meets-quorum (>= participation-rate QUORUM-PERCENTAGE))
            (meets-threshold (>= approval-rate APPROVAL-THRESHOLD))
            (proposal-passed (and meets-quorum meets-threshold))
        )
            (if proposal-passed
                ;; Proposal passed - refund deposit to proposer
                (begin
                    (try! (as-contract (stx-transfer? 
                        (get deposit-amount deposit)
                        tx-sender
                        (get proposer deposit)
                    )))
                    (map-set proposal-deposits proposal-id
                        (merge deposit {
                            refunded: true,
                            processed-at: (some burn-block-height)
                        })
                    )
                    (ok "deposit-refunded")
                )
                ;; Proposal failed - forfeit deposit to DAO treasury
                (begin
                    (map-set proposal-deposits proposal-id
                        (merge deposit {
                            forfeited: true,
                            processed-at: (some burn-block-height)
                        })
                    )
                    ;; Deposit stays in contract (adds to DAO treasury)
                    (var-set total-funds (+ (var-get total-funds) (get deposit-amount deposit)))
                    (ok "deposit-forfeited")
                )
            )
        )
    )
)

;; Read-only function: Get proposal deposit information
(define-read-only (get-proposal-deposit (proposal-id uint))
    (ok (map-get? proposal-deposits proposal-id))
)

(define-constant ERR-CANNOT-DELEGATE-TO-SELF (err u115))
(define-constant ERR-DELEGATION-NOT-FOUND (err u116))
(define-constant ERR-NO-STAKE-TO-DELEGATE (err u117))
(define-constant MAX-DELEGATION-DEPTH u3)

(define-map delegations
    principal
    {
        delegate-to: principal,
        delegated-at: uint,
        active: bool
    }
)

(define-map delegation-power
    principal
    {
        own-power: uint,
        delegated-power: uint,
        total-delegators: uint
    }
)

(define-public (delegate-voting-power (delegate-to principal))
    (let (
        (delegator-stake (default-to u0 (map-get? member-stakes tx-sender)))
        (existing-delegation (map-get? delegations tx-sender))
    )
        (asserts! (not (is-eq tx-sender delegate-to)) ERR-CANNOT-DELEGATE-TO-SELF)
        (asserts! (> delegator-stake u0) ERR-NO-STAKE-TO-DELEGATE)
        
        (match existing-delegation
            prev-delegation
            (let ((prev-delegate (get delegate-to prev-delegation)))
                (if (get active prev-delegation)
                    (let (
                        (prev-delegate-power (default-to {own-power: u0, delegated-power: u0, total-delegators: u0}
                                                        (map-get? delegation-power prev-delegate)))
                        (voting-power (* delegator-stake VOTING_POWER_MULTIPLIER))
                    )
                        (map-set delegation-power prev-delegate {
                            own-power: (get own-power prev-delegate-power),
                            delegated-power: (- (get delegated-power prev-delegate-power) voting-power),
                            total-delegators: (- (get total-delegators prev-delegate-power) u1)
                        })
                        true
                    )
                    true
                )
            )
            true
        )
        
        (map-set delegations tx-sender {
            delegate-to: delegate-to,
            delegated-at: burn-block-height,
            active: true
        })
        
        (let (
            (delegate-power (default-to {own-power: u0, delegated-power: u0, total-delegators: u0}
                                       (map-get? delegation-power delegate-to)))
            (voting-power (* delegator-stake VOTING_POWER_MULTIPLIER))
        )
            (map-set delegation-power delegate-to {
                own-power: (get own-power delegate-power),
                delegated-power: (+ (get delegated-power delegate-power) voting-power),
                total-delegators: (+ (get total-delegators delegate-power) u1)
            })
        )
        
        (ok true)
    )
)

(define-public (revoke-delegation)
    (let (
        (delegation (unwrap! (map-get? delegations tx-sender) ERR-DELEGATION-NOT-FOUND))
        (delegator-stake (default-to u0 (map-get? member-stakes tx-sender)))
    )
        (asserts! (get active delegation) ERR-DELEGATION-NOT-FOUND)
        
        (let (
            (delegate (get delegate-to delegation))
            (delegate-power (default-to {own-power: u0, delegated-power: u0, total-delegators: u0}
                                       (map-get? delegation-power delegate)))
            (voting-power (* delegator-stake VOTING_POWER_MULTIPLIER))
        )
            (map-set delegation-power delegate {
                own-power: (get own-power delegate-power),
                delegated-power: (- (get delegated-power delegate-power) voting-power),
                total-delegators: (- (get total-delegators delegate-power) u1)
            })
        )
        
        (map-set delegations tx-sender
            (merge delegation {active: false})
        )
        
        (ok true)
    )
)

(define-public (vote-with-delegation (proposal-id uint) (vote-yes bool))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (voter-stake (default-to u0 (map-get? member-stakes tx-sender)))
        (delegation-info (map-get? delegations tx-sender))
        (voter-delegation-power (default-to {own-power: u0, delegated-power: u0, total-delegators: u0}
                                           (map-get? delegation-power tx-sender)))
        (current-votes (default-to {yes-votes: u0, no-votes: u0, total-voters: u0}
                                  (map-get? proposal-votes proposal-id)))
    )
        (try! (check-dao-active))
        (asserts! (not (default-to false (map-get? votes {proposal-id: proposal-id, voter: tx-sender}))) ERR-ALREADY-VOTED)
        (asserts! (< burn-block-height (get deadline proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (> voter-stake u0) ERR-NOT-AUTHORIZED)
        
        (match delegation-info
            delegation
            (asserts! (not (get active delegation)) ERR-NOT-AUTHORIZED)
            true
        )
        
        (let (
            (base-voting-power (* voter-stake VOTING_POWER_MULTIPLIER))
            (total-voting-power (+ base-voting-power (get delegated-power voter-delegation-power)))
        )
            (if vote-yes
                (map-set proposal-votes proposal-id {
                    yes-votes: (+ (get yes-votes current-votes) total-voting-power),
                    no-votes: (get no-votes current-votes),
                    total-voters: (+ (get total-voters current-votes) u1)
                })
                (map-set proposal-votes proposal-id {
                    yes-votes: (get yes-votes current-votes),
                    no-votes: (+ (get no-votes current-votes) total-voting-power),
                    total-voters: (+ (get total-voters current-votes) u1)
                })
            )
        )
        
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} true)
        (ok true)
    )
)

(define-read-only (get-delegation-info (member principal))
    (ok (map-get? delegations member))
)

(define-read-only (get-total-voting-power (member principal))
    (let (
        (member-stake (default-to u0 (map-get? member-stakes member)))
        (delegation-info (map-get? delegations member))
        (delegate-power-info (default-to {own-power: u0, delegated-power: u0, total-delegators: u0}
                                        (map-get? delegation-power member)))
    )
        (match delegation-info
            delegation
            (if (get active delegation)
                (ok {base-power: u0, delegated-power: u0, total-power: u0, is-delegating: true})
                (ok {
                    base-power: (* member-stake VOTING_POWER_MULTIPLIER),
                    delegated-power: (get delegated-power delegate-power-info),
                    total-power: (+ (* member-stake VOTING_POWER_MULTIPLIER) (get delegated-power delegate-power-info)),
                    is-delegating: false
                })
            )
            (ok {
                base-power: (* member-stake VOTING_POWER_MULTIPLIER),
                delegated-power: (get delegated-power delegate-power-info),
                total-power: (+ (* member-stake VOTING_POWER_MULTIPLIER) (get delegated-power delegate-power-info)),
                is-delegating: false
            })
        )
    )
)

(define-read-only (get-delegation-stats (member principal))
    (let (
        (delegation-info (map-get? delegations member))
        (delegate-power-info (default-to {own-power: u0, delegated-power: u0, total-delegators: u0}
                                        (map-get? delegation-power member)))
    )
        (ok {
            is-delegating: (match delegation-info
                               delegation (get active delegation)
                               false),
            delegated-to: (match delegation-info
                             delegation (if (get active delegation) (some (get delegate-to delegation)) none)
                             none),
            receives-delegated-power: (get delegated-power delegate-power-info),
            total-delegators: (get total-delegators delegate-power-info)
        })
    )
)