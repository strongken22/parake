;; Advanced Voting with Power Transfer Smart Contract
;; Comprehensive governance system with delegation, reputation, and advanced features

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-POWER (err u102))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u103))
(define-constant ERR-VOTING-CLOSED (err u104))
(define-constant ERR-ALREADY-VOTED (err u105))
(define-constant ERR-SELF-DELEGATION (err u106))
(define-constant ERR-INVALID-PROPOSAL-TYPE (err u107))
(define-constant ERR-QUORUM-NOT-MET (err u108))
(define-constant ERR-INVALID-THRESHOLD (err u109))
(define-constant ERR-DELEGATION-LIMIT-EXCEEDED (err u110))
(define-constant ERR-VOTING-NOT-STARTED (err u111))
(define-constant ERR-INVALID-TIMELOCK (err u112))
(define-constant ERR-EXECUTION-FAILED (err u113))
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED (err u114))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u115))

;; Contract owner and governance
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-DELEGATIONS u50) ;; Maximum number of delegations per voter
(define-constant MAX-VOTING-PERIOD u144000) ;; ~100 days in blocks
(define-constant MIN-VOTING-PERIOD u1440) ;; ~1 day in blocks

;; Data variables
(define-data-var proposal-counter uint u0)
(define-data-var total-voting-power uint u0)
(define-data-var governance-token-contract (optional principal) none)
(define-data-var minimum-quorum uint u1000) ;; Minimum votes needed
(define-data-var proposal-threshold uint u100) ;; Min power to create proposal
(define-data-var execution-delay uint u1440) ;; Timelock delay in blocks

;; Proposal types
(define-constant PROPOSAL-TYPE-STANDARD u0)
(define-constant PROPOSAL-TYPE-CONSTITUTIONAL u1)
(define-constant PROPOSAL-TYPE-EMERGENCY u2)
(define-constant PROPOSAL-TYPE-TREASURY u3)

;; Data maps
;; Enhanced voting power tracking
(define-map voting-power principal uint)
(define-map original-power principal uint) ;; Track original power before delegations
(define-map power-history principal (list 100 {block: uint, power: uint}))

;; Reputation system
(define-map reputation-score principal uint)
(define-map participation-count principal uint)
(define-map voting-streak principal uint)

;; Advanced delegation system
(define-map delegations principal principal)
(define-map delegation-chains principal (list 10 principal)) ;; Track delegation chains
(define-map delegation-expiry principal uint) ;; Temporary delegations
(define-map delegation-limits principal uint) ;; Max power a delegate can receive
(define-map active-delegations principal uint) ;; Count of active delegations

;; Multi-signature and approval system
(define-map multi-sig-proposals uint (list 10 principal)) ;; Required approvers
(define-map multi-sig-approvals {proposal-id: uint, approver: principal} bool)

;; Enhanced proposal system
(define-map proposals uint {
    title: (string-utf8 100),
    description: (string-utf8 500),
    proposal-type: uint,
    creator: principal,
    yes-votes: uint,
    no-votes: uint,
    abstain-votes: uint,
    start-block: uint,
    end-block: uint,
    execution-block: uint,
    quorum-required: uint,
    threshold-percentage: uint,
    active: bool,
    executed: bool,
    cancelled: bool,
    treasury-amount: uint,
    target-contract: (optional principal)
})

;; Voting tracking with more details
(define-map proposal-votes {proposal-id: uint, voter: principal} {
    vote-type: uint, ;; 0=no, 1=yes, 2=abstain
    voting-power: uint,
    block-height: uint,
    reputation-at-vote: uint
})

;; Proposal tags and categories
(define-map proposal-tags uint (list 5 (string-ascii 20)))
(define-map proposals-by-category (string-ascii 20) (list 100 uint))

;; Governance settings
(define-map governance-parameters (string-ascii 50) uint)

;; Treasury and rewards
(define-map treasury-allocations uint {recipient: principal, amount: uint, claimed: bool})
(define-map voting-rewards principal uint)

;; Veto system
(define-data-var veto-council (list 5 principal) (list))
(define-map veto-votes {proposal-id: uint, council-member: principal} bool)

;; Public functions

;; === INITIALIZATION AND POWER MANAGEMENT ===

;; Initialize voter with power and reputation
(define-public (initialize-voter (voter principal) (power uint) (initial-reputation uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> power u0) ERR-INVALID-AMOUNT)
        
        (map-set voting-power voter power)
        (map-set original-power voter power)
        (map-set reputation-score voter initial-reputation)
        (map-set participation-count voter u0)
        (map-set voting-streak voter u0)
        
        ;; Update total voting power
        (var-set total-voting-power (+ (var-get total-voting-power) power))
        
        ;; Record power history
        (map-set power-history voter (list {block: block-height, power: power}))
        
        (ok power)
    )
)

;; Stake tokens to increase voting power (if governance token is set)
(define-public (stake-for-power (amount uint))
    (let (
        (current-power (get-voting-power tx-sender))
        (new-power (+ current-power amount))
    )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        
        ;; Update voting power
        (map-set voting-power tx-sender new-power)
        (var-set total-voting-power (+ (var-get total-voting-power) amount))
        
        ;; Update power history
        (let ((current-history (default-to (list) (map-get? power-history tx-sender))))
            (map-set power-history tx-sender 
                (unwrap! (as-max-len? (append current-history {block: block-height, power: new-power}) u100) 
                    ERR-INVALID-AMOUNT))
        )
        
        (ok new-power)
    )
)

;; === ADVANCED DELEGATION SYSTEM ===

;; Delegate with expiry and limits
(define-public (delegate-voting-power-advanced (delegate principal) (expiry-block uint) (max-power uint))
    (let (
        (delegator tx-sender)
        (current-power (get-voting-power delegator))
        (delegate-current-delegations (default-to u0 (map-get? active-delegations delegate)))
    )
        (asserts! (not (is-eq delegator delegate)) ERR-SELF-DELEGATION)
        (asserts! (> current-power u0) ERR-INSUFFICIENT-POWER)
        (asserts! (< delegate-current-delegations MAX-DELEGATIONS) ERR-DELEGATION-LIMIT-EXCEEDED)
        (asserts! (> expiry-block block-height) ERR-INVALID-TIMELOCK)
        
        ;; Set delegation limits
        (if (> max-power u0)
            (map-set delegation-limits delegate max-power)
            true
        )
        
        ;; Transfer power
        (map-set voting-power delegator u0)
        (map-set voting-power delegate (+ (get-voting-power delegate) current-power))
        
        ;; Record delegation with expiry
        (map-set delegations delegator delegate)
        (map-set delegation-expiry delegator expiry-block)
        
        ;; Update delegation count
        (map-set active-delegations delegate (+ delegate-current-delegations u1))
        
        ;; Build delegation chain
        (let ((current-chain (default-to (list) (map-get? delegation-chains delegator))))
            (map-set delegation-chains delegator 
                (unwrap! (as-max-len? (append current-chain delegate) u10) ERR-DELEGATION-LIMIT-EXCEEDED))
        )
        
        (ok current-power)
    )
)

;; Auto-revoke expired delegations
(define-public (revoke-expired-delegations (delegators (list 10 principal)))
    (let (
        (results (map revoke-if-expired delegators))
        (successful-revocations (fold count-successful-revocations results u0))
    )
        (ok successful-revocations)
    )
)

;; Batch delegation for multiple voters
(define-public (batch-delegate (delegates (list 10 {voter: principal, delegate: principal})))
    (let (
        (results (map process-delegation delegates))
        (successful-delegations (fold count-successful-delegations results u0))
    )
        (ok successful-delegations)
    )
)

;; === ENHANCED PROPOSAL SYSTEM ===

;; Create proposal with advanced options
(define-public (create-advanced-proposal 
    (title (string-utf8 100)) 
    (description (string-utf8 500))
    (proposal-type uint)
    (voting-period uint)
    (quorum-required uint)
    (threshold-percentage uint)
    (tags (list 5 (string-ascii 20)))
    (treasury-amount uint)
    (target-contract (optional principal))
    (multi-sig-approvers (list 10 principal))
)
    (let (
        (proposal-id (+ (var-get proposal-counter) u1))
        (creator-power (get-voting-power tx-sender))
        (creator-reputation (get-reputation tx-sender))
        (required-power (var-get proposal-threshold))
        (start-block (+ block-height u144)) ;; Start voting in ~6 hours
        (end-block (+ start-block voting-period))
        (execution-block (+ end-block (var-get execution-delay)))
    )
        ;; Validate proposal creation rights
        (asserts! (>= creator-power required-power) ERR-INSUFFICIENT-POWER)
        (asserts! (>= creator-reputation u50) ERR-INSUFFICIENT-REPUTATION)
        (asserts! (<= proposal-type u3) ERR-INVALID-PROPOSAL-TYPE)
        (asserts! (and (>= voting-period MIN-VOTING-PERIOD) (<= voting-period MAX-VOTING-PERIOD)) ERR-INVALID-AMOUNT)
        (asserts! (and (>= threshold-percentage u1) (<= threshold-percentage u100)) ERR-INVALID-THRESHOLD)
        
        ;; Create proposal
        (map-set proposals proposal-id {
            title: title,
            description: description,
            proposal-type: proposal-type,
            creator: tx-sender,
            yes-votes: u0,
            no-votes: u0,
            abstain-votes: u0,
            start-block: start-block,
            end-block: end-block,
            execution-block: execution-block,
            quorum-required: quorum-required,
            threshold-percentage: threshold-percentage,
            active: true,
            executed: false,
            cancelled: false,
            treasury-amount: treasury-amount,
            target-contract: target-contract
        })
        
        ;; Set tags
        (map-set proposal-tags proposal-id tags)
        
        ;; Set multi-sig approvers if provided
        (if (> (len multi-sig-approvers) u0)
            (map-set multi-sig-proposals proposal-id multi-sig-approvers)
            true
        )
        
        (var-set proposal-counter proposal-id)
        (ok proposal-id)
    )
)

;; Vote with reputation bonus
(define-public (vote-with-reputation (proposal-id uint) (vote-type uint))
    (let (
        (voter tx-sender)
        (voter-power (get-voting-power voter))
        (voter-reputation (get-reputation voter))
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (reputation-bonus (/ voter-reputation u10)) ;; 10% bonus per 100 reputation
        (effective-power (+ voter-power reputation-bonus))
    )
        ;; Validate voting conditions
        (asserts! (get active proposal) ERR-VOTING-CLOSED)
        (asserts! (>= block-height (get start-block proposal)) ERR-VOTING-NOT-STARTED)
        (asserts! (< block-height (get end-block proposal)) ERR-VOTING-CLOSED)
        (asserts! (is-none (map-get? proposal-votes {proposal-id: proposal-id, voter: voter})) ERR-ALREADY-VOTED)
        (asserts! (> voter-power u0) ERR-INSUFFICIENT-POWER)
        (asserts! (<= vote-type u2) ERR-INVALID-AMOUNT) ;; 0=no, 1=yes, 2=abstain
        
        ;; Record vote with details
        (map-set proposal-votes {proposal-id: proposal-id, voter: voter} {
            vote-type: vote-type,
            voting-power: effective-power,
            block-height: block-height,
            reputation-at-vote: voter-reputation
        })
        
        ;; Update proposal vote counts based on vote type
        (if (is-eq vote-type u1)
            ;; Yes vote
            (map-set proposals proposal-id 
                (merge proposal {yes-votes: (+ (get yes-votes proposal) effective-power)}))
            ;; No or abstain vote
            (if (is-eq vote-type u0)
                ;; No vote
                (map-set proposals proposal-id 
                    (merge proposal {no-votes: (+ (get no-votes proposal) effective-power)}))
                ;; Abstain vote
                (map-set proposals proposal-id 
                    (merge proposal {abstain-votes: (+ (get abstain-votes proposal) effective-power)}))
            )
        )
        
        ;; Update voter stats
        (update-voter-participation voter)
        
        (ok effective-power)
    )
)

;; Multi-signature approval
(define-public (multi-sig-approve (proposal-id uint))
    (let (
        (approvers (unwrap! (map-get? multi-sig-proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (is-approver (is-some (index-of approvers tx-sender)))
    )
        (asserts! is-approver ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? multi-sig-approvals {proposal-id: proposal-id, approver: tx-sender})) ERR-ALREADY-VOTED)
        
        (map-set multi-sig-approvals {proposal-id: proposal-id, approver: tx-sender} true)
        (ok true)
    )
)

;; Execute proposal with advanced checks
(define-public (execute-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (total-votes (+ (+ (get yes-votes proposal) (get no-votes proposal)) (get abstain-votes proposal)))
        (yes-percentage (if (> total-votes u0) (/ (* (get yes-votes proposal) u100) total-votes) u0))
        (quorum-met (>= total-votes (get quorum-required proposal)))
        (threshold-met (>= yes-percentage (get threshold-percentage proposal)))
        (multi-sig-required (is-some (map-get? multi-sig-proposals proposal-id)))
    )
        ;; Validate execution conditions
        (asserts! (not (get executed proposal)) ERR-PROPOSAL-ALREADY-EXECUTED)
        (asserts! (not (get cancelled proposal)) ERR-VOTING-CLOSED)
        (asserts! (>= block-height (get execution-block proposal)) ERR-INVALID-TIMELOCK)
        (asserts! quorum-met ERR-QUORUM-NOT-MET)
        (asserts! threshold-met ERR-INVALID-THRESHOLD)
        
        ;; Check multi-sig approvals if required
        (if multi-sig-required
            (asserts! (check-multi-sig-approvals proposal-id) ERR-NOT-AUTHORIZED)
            true
        )
        
        ;; Mark as executed
        (map-set proposals proposal-id (merge proposal {executed: true}))
        
        ;; Execute treasury allocation if specified
        (if (> (get treasury-amount proposal) u0)
            (map-set treasury-allocations proposal-id {
                recipient: (get creator proposal),
                amount: (get treasury-amount proposal),
                claimed: false
            })
            true
        )
        
        ;; Distribute voting rewards
        (distribute-voting-rewards proposal-id)
        
        (ok true)
    )
)

;; === GOVERNANCE AND ADMIN ===

;; Set veto council members (only owner)
(define-public (set-veto-council (members (list 5 principal)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set veto-council members)
        (ok true)
    )
)

;; Emergency cancel proposal (veto council)
(define-public (emergency-cancel (proposal-id uint))
    (let (
        (council (var-get veto-council))
        (is-council-member (is-some (index-of council tx-sender)))
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
        (asserts! is-council-member ERR-NOT-AUTHORIZED)
        (asserts! (get active proposal) ERR-VOTING-CLOSED)
        
        (map-set proposals proposal-id (merge proposal {cancelled: true, active: false}))
        (ok true)
    )
)

;; Update governance parameters
(define-public (update-governance-parameter (parameter (string-ascii 50)) (value uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set governance-parameters parameter value)
        (ok true)
    )
)

;; === READ-ONLY FUNCTIONS ===

;; Get comprehensive voter info
(define-read-only (get-voter-info (voter principal))
    (ok {
        voting-power: (get-voting-power voter),
        original-power: (default-to u0 (map-get? original-power voter)),
        reputation: (get-reputation voter),
        participation-count: (default-to u0 (map-get? participation-count voter)),
        voting-streak: (default-to u0 (map-get? voting-streak voter)),
        active-delegations: (default-to u0 (map-get? active-delegations voter)),
        delegation-target: (map-get? delegations voter),
        delegation-expiry: (map-get? delegation-expiry voter)
    })
)

;; Get proposal with comprehensive details
(define-read-only (get-proposal-details (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal (ok {
            proposal: proposal,
            tags: (default-to (list) (map-get? proposal-tags proposal-id)),
            multi-sig-approvers: (map-get? multi-sig-proposals proposal-id),
            total-votes: (+ (+ (get yes-votes proposal) (get no-votes proposal)) (get abstain-votes proposal)),
            yes-percentage: (if (> (+ (+ (get yes-votes proposal) (get no-votes proposal)) (get abstain-votes proposal)) u0)
                (/ (* (get yes-votes proposal) u100) (+ (+ (get yes-votes proposal) (get no-votes proposal)) (get abstain-votes proposal)))
                u0),
            quorum-status: (>= (+ (+ (get yes-votes proposal) (get no-votes proposal)) (get abstain-votes proposal)) (get quorum-required proposal)),
            can-execute: (and 
                (>= block-height (get execution-block proposal))
                (not (get executed proposal))
                (not (get cancelled proposal))
            )
        })
        ERR-PROPOSAL-NOT-FOUND
    )
)

;; Get veto council members
(define-read-only (get-veto-council)
    (var-get veto-council)
)

;; Get governance statistics
(define-read-only (get-governance-stats)
    (ok {
        total-voting-power: (var-get total-voting-power),
        total-proposals: (var-get proposal-counter),
        minimum-quorum: (var-get minimum-quorum),
        proposal-threshold: (var-get proposal-threshold),
        execution-delay: (var-get execution-delay),
        veto-council-size: (len (var-get veto-council))
    })
)

;; === HELPER FUNCTIONS ===

;; Get voting power (same as before)
(define-read-only (get-voting-power (voter principal))
    (default-to u0 (map-get? voting-power voter))
)

;; Get reputation score
(define-read-only (get-reputation (voter principal))
    (default-to u0 (map-get? reputation-score voter))
)

;; Update voter participation and reputation
(define-private (update-voter-participation (voter principal))
    (let (
        (current-participation (default-to u0 (map-get? participation-count voter)))
        (current-streak (default-to u0 (map-get? voting-streak voter)))
        (current-reputation (get-reputation voter))
    )
        (map-set participation-count voter (+ current-participation u1))
        (map-set voting-streak voter (+ current-streak u1))
        (map-set reputation-score voter (+ current-reputation u5)) ;; 5 reputation per vote
        true
    )
)

;; Check if multi-sig approvals are sufficient
(define-private (check-multi-sig-approvals (proposal-id uint))
    (let (
        (approvers (unwrap! (map-get? multi-sig-proposals proposal-id) false))
        (required-approvals (/ (len approvers) u2)) ;; 50% of approvers needed
        (actual-approvals (fold count-approval-fold approvers {proposal-id: proposal-id, count: u0}))
    )
        (>= (get count actual-approvals) required-approvals)
    )
)

;; Count multi-sig approvals using fold
(define-private (count-approval-fold (approver principal) (acc {proposal-id: uint, count: uint}))
    (let (
        (proposal-id (get proposal-id acc))
        (current-count (get count acc))
        (has-approval (is-some (map-get? multi-sig-approvals {proposal-id: proposal-id, approver: approver})))
    )
        {
            proposal-id: proposal-id,
            count: (if has-approval (+ current-count u1) current-count)
        }
    )
)

;; Distribute voting rewards
(define-private (distribute-voting-rewards (proposal-id uint))
    ;; Simplified reward distribution - would need more complex logic
    true
)

;; Helper for checking expiry
(define-private (revoke-if-expired (voter principal))
    (match (map-get? delegation-expiry voter)
        expiry-block (if (<= expiry-block block-height)
            (ok (revoke-delegation-internal voter))
            (ok false))
        (ok false)
    )
)

;; Internal delegation revocation
(define-private (revoke-delegation-internal (delegator principal))
    (match (map-get? delegations delegator)
        delegate (let (
            (delegator-original-power (default-to u0 (map-get? original-power delegator)))
            (current-delegate-power (get-voting-power delegate))
            (current-delegations (default-to u0 (map-get? active-delegations delegate)))
        )
            (map-delete delegations delegator)
            (map-delete delegation-expiry delegator)
            (map-set voting-power delegator delegator-original-power)
            (map-set voting-power delegate (- current-delegate-power delegator-original-power))
            (map-set active-delegations delegate (- current-delegations u1))
            true
        )
        false
    )
)

;; Process batch delegation
(define-private (process-delegation (delegation {voter: principal, delegate: principal}))
    ;; Simplified - would need full validation
    (ok true)
)

;; Helper functions for counting successful operations
(define-private (count-successful-revocations (result (response bool uint)) (count uint))
    (if (is-ok result) (+ count u1) count)
)

(define-private (count-successful-delegations (result (response bool uint)) (count uint))
    (if (is-ok result) (+ count u1) count)
)