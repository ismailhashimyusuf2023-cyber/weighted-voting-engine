;; ============================================================
;; Contract: weighted-voting-engine
;; Purpose : Weighted voting with threshold-based finalization
;; ============================================================

;; ------------------------------------------------------------
;; Error Codes
;; ------------------------------------------------------------
(define-constant ERR-PROPOSAL-NOT-FOUND   (err u4500))
(define-constant ERR-ALREADY-VOTED        (err u4501))
(define-constant ERR-ALREADY-FINALIZED    (err u4502))
(define-constant ERR-INVALID-PARAMS       (err u4503))

;; ------------------------------------------------------------
;; Data Variables
;; ------------------------------------------------------------
(define-data-var proposal-counter uint u0)

;; ------------------------------------------------------------
;; Maps
;; ------------------------------------------------------------

(define-map proposals
  { id: uint }
  {
    threshold: uint,
    score: uint,
    finalized: bool
  }
)

(define-map votes
  { id: uint, voter: principal }
  { weight: uint }
)

;; ------------------------------------------------------------
;; Read-Only Functions
;; ------------------------------------------------------------

(define-read-only (get-proposal (id uint))
  (map-get? proposals { id: id })
)

(define-read-only (has-voted (id uint) (voter principal))
  (is-some (map-get? votes { id: id, voter: voter }))
)

(define-read-only (get-score (id uint))
  (default-to u0
    (get score (map-get? proposals { id: id }))
  )
)

(define-read-only (is-finalized (id uint))
  (default-to false
    (get finalized (map-get? proposals { id: id }))
  )
)

;; ------------------------------------------------------------
;; Internal Helper
;; ------------------------------------------------------------

(define-private (next-proposal-id)
  (let ((id (var-get proposal-counter)))
    (var-set proposal-counter (+ id u1))
    id
  )
)

;; ------------------------------------------------------------
;; Public Functions - Proposal Creation
;; ------------------------------------------------------------

(define-public (create-proposal (threshold uint))
  (begin
    (asserts! (> threshold u0) ERR-INVALID-PARAMS)

    (let ((id (next-proposal-id)))
      (map-set proposals
        { id: id }
        {
          threshold: threshold,
          score: u0,
          finalized: false
        }
      )
      (ok id)
    )
  )
)

;; ------------------------------------------------------------
;; Public Functions - Voting
;; ------------------------------------------------------------

(define-public (vote (id uint) (weight uint))
  (match (map-get? proposals { id: id })
    p
      (begin
        (asserts! (> weight u0) ERR-INVALID-PARAMS)
        (asserts! (not (get finalized p)) ERR-ALREADY-FINALIZED)
        (asserts! (not (has-voted id tx-sender)) ERR-ALREADY-VOTED)

        ;; Record vote
        (map-set votes
          { id: id, voter: tx-sender }
          { weight: weight }
        )

        ;; Update score
        (let ((new-score (+ (get score p) weight)))
          (map-set proposals
            { id: id }
            {
              threshold: (get threshold p),
              score: new-score,
              finalized: (>= new-score (get threshold p))
            }
          )
        )

        (ok true)
      )
    ERR-PROPOSAL-NOT-FOUND
  )
)
