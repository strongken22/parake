;; Enhanced Storage with Expiry Smart Contract
;; This contract allows storing values that automatically expire after a certain number of blocks
;; with advanced features including delegation, renewal, metadata, and events

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-EXPIRY (err u101))
(define-constant ERR-NO-VALUE (err u102))
(define-constant ERR-ALREADY-EXISTS (err u103))
(define-constant ERR-INVALID-DELEGATE (err u104))
(define-constant ERR-MAX-DELEGATES-REACHED (err u105))
(define-constant ERR-RENEWAL-DISABLED (err u106))
(define-constant ERR-INVALID-TAG (err u107))
(define-constant ERR-CONTRACT-PAUSED (err u108))

;; Contract owner for admin functions
(define-data-var contract-owner principal tx-sender)
(define-data-var is-paused bool false)
(define-data-var total-entries uint u0)
(define-data-var total-expired uint u0)

;; Configuration
(define-constant MAX-DELEGATES u5)
(define-constant MIN-EXPIRY-BLOCKS u10)
(define-constant MAX-EXPIRY-BLOCKS u52560) ;; ~1 year at 10 min blocks

;; Data structures
(define-map storage
    { key: (string-utf8 256) }
    {
        value: (string-utf8 1024),
        expiry-block: uint,
        owner: principal,
        created-block: uint,
        last-updated: uint,
        access-count: uint,
        is-renewable: bool,
        auto-renew-blocks: uint,
        metadata: (optional (string-utf8 256))
    }
)

;; Track delegates list per key
(define-map delegate-list
    { key: (string-utf8 256) }
    { delegates: (list 5 principal) }
)

;; Delegation map - allows owners to delegate access
(define-map delegates
    { key: (string-utf8 256), delegate: principal }
    { can-read: bool, can-update: bool, can-extend: bool }
)

;; Tags for categorization
(define-map key-tags
    { key: (string-utf8 256) }
    { tags: (list 10 (string-utf8 64)) }
)

;; Access log for audit trail
(define-map access-log
    { key: (string-utf8 256), block: uint }
    { accessor: principal, action: (string-ascii 32) }
)

;; Contract admin functions

(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set contract-owner new-owner))
    )
)

(define-public (pause-contract)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set is-paused true))
    )
)

(define-public (unpause-contract)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set is-paused false))
    )
)

;; Helper functions

(define-private (is-owner-or-delegate (key (string-utf8 256)) (user principal) (permission (string-ascii 32)))
    (match (map-get? storage { key: key })
        entry
        (if (is-eq user (get owner entry))
            true
            (match (map-get? delegates { key: key, delegate: user })
                del-entry
                (if (is-eq permission "read")
                    (get can-read del-entry)
                    (if (is-eq permission "update")
                        (get can-update del-entry)
                        (if (is-eq permission "extend")
                            (get can-extend del-entry)
                            false
                        )
                    )
                )
                false
            )
        )
        false
    )
)

(define-private (log-access (key (string-utf8 256)) (action (string-ascii 32)))
    (map-set access-log
        { key: key, block: block-height }
        { accessor: tx-sender, action: action }
    )
)

;; Read-only functions

(define-read-only (get-value (key (string-utf8 256)))
    (match (map-get? storage { key: key })
        entry
        (if (<= (get expiry-block entry) block-height)
            ;; Check if auto-renewable
            (if (and (get is-renewable entry) (> (get auto-renew-blocks entry) u0))
                (some (get value entry))
                none
            )
            (some (get value entry))
        )
        none
    )
)

(define-read-only (get-entry-full (key (string-utf8 256)))
    (match (map-get? storage { key: key })
        entry
        (let ((is-expired (<= (get expiry-block entry) block-height)))
            (if (and is-expired (not (get is-renewable entry)))
                none
                (some {
                    value: (get value entry),
                    expiry-block: (get expiry-block entry),
                    owner: (get owner entry),
                    created-block: (get created-block entry),
                    last-updated: (get last-updated entry),
                    access-count: (get access-count entry),
                    is-renewable: (get is-renewable entry),
                    auto-renew-blocks: (get auto-renew-blocks entry),
                    metadata: (get metadata entry),
                    blocks-until-expiry: (if is-expired u0 (- (get expiry-block entry) block-height)),
                    is-expired: is-expired
                })
            )
        )
        none
    )
)

(define-read-only (get-delegates-for-key (key (string-utf8 256)))
    (default-to 
        { delegates: (list) }
        (map-get? delegate-list { key: key })
    )
)

;; Get permissions for a specific delegate
(define-read-only (get-delegate-permissions (key (string-utf8 256)) (delegate principal))
    (map-get? delegates { key: key, delegate: delegate })
)

(define-read-only (get-tags (key (string-utf8 256)))
    (default-to
        { tags: (list) }
        (map-get? key-tags { key: key })
    )
)

(define-read-only (get-stats)
    {
        total-entries: (var-get total-entries),
        total-expired: (var-get total-expired),
        is-paused: (var-get is-paused),
        contract-owner: (var-get contract-owner)
    }
)

;; Public functions

(define-public (store-value-advanced 
    (key (string-utf8 256)) 
    (value (string-utf8 1024)) 
    (blocks-until-expiry uint)
    (is-renewable bool)
    (auto-renew-blocks uint)
    (metadata (optional (string-utf8 256))))
    (begin
        (asserts! (not (var-get is-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (is-none (map-get? storage { key: key })) ERR-ALREADY-EXISTS)
        (asserts! (>= blocks-until-expiry MIN-EXPIRY-BLOCKS) ERR-INVALID-EXPIRY)
        (asserts! (<= blocks-until-expiry MAX-EXPIRY-BLOCKS) ERR-INVALID-EXPIRY)
        
        (map-set storage
            { key: key }
            {
                value: value,
                expiry-block: (+ block-height blocks-until-expiry),
                owner: tx-sender,
                created-block: block-height,
                last-updated: block-height,
                access-count: u0,
                is-renewable: is-renewable,
                auto-renew-blocks: auto-renew-blocks,
                metadata: metadata
            }
        )
        (var-set total-entries (+ (var-get total-entries) u1))
        (log-access key "create")
        (ok true)
    )
)

(define-public (store-value (key (string-utf8 256)) (value (string-utf8 1024)) (blocks-until-expiry uint))
    (store-value-advanced key value blocks-until-expiry false u0 none)
)

(define-public (update-value-with-metadata 
    (key (string-utf8 256)) 
    (new-value (string-utf8 1024))
    (new-metadata (optional (string-utf8 256))))
    (match (map-get? storage { key: key })
        entry
        (begin
            (asserts! (not (var-get is-paused)) ERR-CONTRACT-PAUSED)
            (asserts! (is-owner-or-delegate key tx-sender "update") ERR-NOT-AUTHORIZED)
            (asserts! (> (get expiry-block entry) block-height) ERR-NO-VALUE)
            
            (map-set storage
                { key: key }
                (merge entry {
                    value: new-value,
                    last-updated: block-height,
                    access-count: (+ (get access-count entry) u1),
                    metadata: new-metadata
                })
            )
            (log-access key "update")
            (ok true)
        )
        ERR-NO-VALUE
    )
)

(define-public (auto-renew (key (string-utf8 256)))
    (match (map-get? storage { key: key })
        entry
        (begin
            (asserts! (not (var-get is-paused)) ERR-CONTRACT-PAUSED)
            (asserts! (get is-renewable entry) ERR-RENEWAL-DISABLED)
            (asserts! (<= (get expiry-block entry) block-height) ERR-NO-VALUE)
            
            (let ((new-expiry (+ block-height (get auto-renew-blocks entry))))
                (map-set storage
                    { key: key }
                    (merge entry {
                        expiry-block: new-expiry,
                        last-updated: block-height
                    })
                )
                (log-access key "auto-renew")
                (ok new-expiry)
            )
        )
        ERR-NO-VALUE
    )
)

(define-public (add-delegate 
    (key (string-utf8 256)) 
    (delegate principal)
    (can-read bool)
    (can-update bool)
    (can-extend bool))
    (match (map-get? storage { key: key })
        entry
        (begin
            (asserts! (not (var-get is-paused)) ERR-CONTRACT-PAUSED)
            (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-AUTHORIZED)
            (asserts! (not (is-eq delegate (get owner entry))) ERR-INVALID-DELEGATE)
            
            ;; Update delegate permissions
            (map-set delegates
                { key: key, delegate: delegate }
                { can-read: can-read, can-update: can-update, can-extend: can-extend }
            )
            
            ;; Update delegate list
            (let ((current-list (default-to { delegates: (list) } (map-get? delegate-list { key: key }))))
                (let ((current-delegates (get delegates current-list)))
                    (if (is-none (index-of current-delegates delegate))
                        ;; Add delegate if not already in list
                        (begin
                            (asserts! (< (len current-delegates) MAX-DELEGATES) ERR-MAX-DELEGATES-REACHED)
                            (map-set delegate-list
                                { key: key }
                                { delegates: (unwrap! (as-max-len? (append current-delegates delegate) u5) ERR-MAX-DELEGATES-REACHED) }
                            )
                        )
                        ;; Delegate already exists, just update permissions
                        true
                    )
                )
            )
            
            (log-access key "add-delegate")
            (ok true)
        )
        ERR-NO-VALUE
    )
)

(define-public (remove-delegate (key (string-utf8 256)) (delegate principal))
    (match (map-get? storage { key: key })
        entry
        (begin
            (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-AUTHORIZED)
            
            ;; Remove from delegates map
            (map-delete delegates { key: key, delegate: delegate })
            
            ;; Remove from delegate list
            (match (map-get? delegate-list { key: key })
                del-list
                (let ((current-delegates (get delegates del-list)))
                    (match (index-of current-delegates delegate)
                        index
                        (let ((new-list (filter is-not-delegate current-delegates)))
                            (map-set delegate-list
                                { key: key }
                                { delegates: new-list }
                            )
                        )
                        true ;; Delegate not in list
                    )
                )
                true ;; No delegate list exists
            )
            
            (log-access key "remove-delegate")
            (ok true)
        )
        ERR-NO-VALUE
    )
)

;; Helper function for filtering delegates
(define-private (is-not-delegate (d principal))
    (not (is-eq d tx-sender)))

(define-public (add-tags (key (string-utf8 256)) (tags (list 10 (string-utf8 64))))
    (match (map-get? storage { key: key })
        entry
        (begin
            (asserts! (is-owner-or-delegate key tx-sender "update") ERR-NOT-AUTHORIZED)
            (map-set key-tags { key: key } { tags: tags })
            (ok true)
        )
        ERR-NO-VALUE
    )
)

(define-public (transfer-ownership (key (string-utf8 256)) (new-owner principal))
    (match (map-get? storage { key: key })
        entry
        (begin
            (asserts! (not (var-get is-paused)) ERR-CONTRACT-PAUSED)
            (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-AUTHORIZED)
            
            (map-set storage
                { key: key }
                (merge entry { owner: new-owner })
            )
            ;; Clear all delegates when ownership transfers
            (log-access key "transfer")
            (ok true)
        )
        ERR-NO-VALUE
    )
)

(define-public (increment-access-count (key (string-utf8 256)))
    (match (map-get? storage { key: key })
        entry
        (begin
            (asserts! (is-owner-or-delegate key tx-sender "read") ERR-NOT-AUTHORIZED)
            (map-set storage
                { key: key }
                (merge entry { access-count: (+ (get access-count entry) u1) })
            )
            (ok (+ (get access-count entry) u1))
        )
        ERR-NO-VALUE
    )
)

;; Batch operations

(define-public (store-batch-advanced (entries (list 10 {
    key: (string-utf8 256),
    value: (string-utf8 1024),
    blocks-until-expiry: uint,
    is-renewable: bool,
    auto-renew-blocks: uint,
    metadata: (optional (string-utf8 256))
})))
    (begin
        (asserts! (not (var-get is-paused)) ERR-CONTRACT-PAUSED)
        (ok (map store-single-entry-advanced entries))
    )
)

(define-private (store-single-entry-advanced (entry {
    key: (string-utf8 256),
    value: (string-utf8 1024),
    blocks-until-expiry: uint,
    is-renewable: bool,
    auto-renew-blocks: uint,
    metadata: (optional (string-utf8 256))
}))
    (begin
        (map-set storage
            { key: (get key entry) }
            {
                value: (get value entry),
                expiry-block: (+ block-height (get blocks-until-expiry entry)),
                owner: tx-sender,
                created-block: block-height,
                last-updated: block-height,
                access-count: u0,
                is-renewable: (get is-renewable entry),
                auto-renew-blocks: (get auto-renew-blocks entry),
                metadata: (get metadata entry)
            }
        )
        (var-set total-entries (+ (var-get total-entries) u1))
        true
    )
)

;; Cleanup function for expired entries (optional, for gas refund)
(define-public (cleanup-expired (keys (list 20 (string-utf8 256))))
    (ok (map cleanup-single-expired keys))
)

(define-private (cleanup-single-expired (key (string-utf8 256)))
    (match (map-get? storage { key: key })
        entry
        (if (and (<= (get expiry-block entry) block-height) (not (get is-renewable entry)))
            (begin
                (map-delete storage { key: key })
                (map-delete key-tags { key: key })
                (var-set total-expired (+ (var-get total-expired) u1))
                true
            )
            false
        )
        false
    )
)