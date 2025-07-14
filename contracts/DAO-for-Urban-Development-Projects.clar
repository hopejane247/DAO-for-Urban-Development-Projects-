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