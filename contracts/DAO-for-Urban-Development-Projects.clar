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
