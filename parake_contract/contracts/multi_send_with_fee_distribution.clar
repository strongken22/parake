;; Multi-Send with Fee Distribution Contract
;; Allows users to send STX to multiple recipients while collecting platform fees

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_EMPTY_RECIPIENTS (err u103))
(define-constant ERR_TRANSFER_FAILED (err u104))
(define-constant ERR_INVALID_FEE_RATE (err u105))

;; Data Variables
(define-data-var fee-rate uint u250) ;; Fee rate in basis points (250 = 2.5%)
(define-data-var accumulated-fees uint u0)
(define-data-var total-volume uint u0)

;; Data Maps
(define-map authorized-owners principal bool)
(define-map owner-fee-shares principal uint) ;; Share percentages in basis points
(define-map withdrawal-history principal uint)

;; Initialize contract owner
(map-set authorized-owners CONTRACT_OWNER true)
(map-set owner-fee-shares CONTRACT_OWNER u10000) ;; 100% initially

;; Read-only functions
(define-read-only (get-fee-rate)
  (var-get fee-rate)
)

(define-read-only (get-accumulated-fees)
  (var-get accumulated-fees)
)

(define-read-only (get-total-volume)
  (var-get total-volume)
)

(define-read-only (is-authorized-owner (owner principal))
  (default-to false (map-get? authorized-owners owner))
)

(define-read-only (get-owner-fee-share (owner principal))
  (default-to u0 (map-get? owner-fee-shares owner))
)

(define-read-only (get-withdrawal-history (owner principal))
  (default-to u0 (map-get? withdrawal-history owner))
)

(define-read-only (calculate-fee (amount uint))
  (/ (* amount (var-get fee-rate)) u10000)
)

(define-read-only (calculate-owner-fee-amount (owner principal))
  (let ((share (get-owner-fee-share owner))
        (total-fees (var-get accumulated-fees)))
    (/ (* total-fees share) u10000))
)

;; Private functions
(define-private (send-stx-to-recipient (recipient {address: principal, amount: uint}))
  (let ((address (get address recipient))
        (amount (get amount recipient)))
    (if (> amount u0)
      (stx-transfer? amount tx-sender address)
      (ok true)))
)

(define-private (sum-amounts (recipient {address: principal, amount: uint}) (sum uint))
  (+ sum (get amount recipient))
)

;; Public functions

;; Add or update authorized owner with fee share
(define-public (add-owner (new-owner principal) (fee-share uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= fee-share u10000) ERR_INVALID_FEE_RATE)
    (map-set authorized-owners new-owner true)
    (map-set owner-fee-shares new-owner fee-share)
    (ok true))
)

;; Remove authorized owner
(define-public (remove-owner (owner principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq owner CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (map-delete authorized-owners owner)
    (map-delete owner-fee-shares owner)
    (ok true))
)

;; Update fee rate (only contract owner)
(define-public (set-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-rate u1000) ERR_INVALID_FEE_RATE) ;; Max 10%
    (var-set fee-rate new-rate)
    (ok true))
)

;; Multi-send function with fee collection
(define-public (multi-send (recipients (list 50 {address: principal, amount: uint})))
  (let ((total-amount (fold sum-amounts recipients u0))
        (fee-amount (calculate-fee total-amount))
        (total-required (+ total-amount fee-amount)))
    (begin
      ;; Validation checks
      (asserts! (> (len recipients) u0) ERR_EMPTY_RECIPIENTS)
      (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
      (asserts! (>= (stx-get-balance tx-sender) total-required) ERR_INSUFFICIENT_BALANCE)
      
      ;; Process all transfers
      (let ((transfer-results (map send-stx-to-recipient recipients)))
        (begin
          ;; Update contract state
          (var-set accumulated-fees (+ (var-get accumulated-fees) fee-amount))
          (var-set total-volume (+ (var-get total-volume) total-amount))
          
          ;; Transfer fee to contract
          (if (> fee-amount u0)
            (match (stx-transfer? fee-amount tx-sender (as-contract tx-sender))
              success-fee (ok {
                total-sent: total-amount,
                fee-collected: fee-amount,
                recipients-count: (len recipients)
              })
              error-fee ERR_TRANSFER_FAILED)
            (ok {
              total-sent: total-amount,
              fee-collected: fee-amount,
              recipients-count: (len recipients)
            }))))))
)

;; Withdraw accumulated fees (for authorized owners)
(define-public (withdraw-fees)
  (let ((caller tx-sender)
        (owner-fee-amount (calculate-owner-fee-amount caller)))
    (begin
      (asserts! (is-authorized-owner caller) ERR_UNAUTHORIZED)
      (asserts! (> owner-fee-amount u0) ERR_INSUFFICIENT_BALANCE)
      
      ;; Transfer fees to owner
      (match (as-contract (stx-transfer? owner-fee-amount tx-sender caller))
        success (begin
          ;; Update withdrawal history
          (map-set withdrawal-history caller 
            (+ (get-withdrawal-history caller) owner-fee-amount))
          ;; Reduce accumulated fees
          (var-set accumulated-fees (- (var-get accumulated-fees) owner-fee-amount))
          (ok owner-fee-amount))
        error ERR_TRANSFER_FAILED)))
)

;; Emergency withdraw (only contract owner)
(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= amount (stx-get-balance (as-contract tx-sender))) ERR_INSUFFICIENT_BALANCE)
    (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
)

;; Private function to process a single batch
(define-private (process-single-batch (recipients (list 50 {address: principal, amount: uint})))
  (multi-send recipients)
)

;; Batch multi-send for efficiency
(define-public (batch-multi-send (batch-recipients (list 10 (list 50 {address: principal, amount: uint}))))
  (ok (map process-single-batch batch-recipients))
)