;; Intellectual Property Rights Management
;; Enables creators to register, manage, and monetize intellectual property rights
;; with transparent licensing, royalty distribution, and usage tracking

;; Define NFT trait locally instead of importing from an external contract
(define-trait nft-trait
  (
    ;; Last token ID, limited to uint range
    (get-last-token-id () (response uint uint))
    ;; URI for metadata associated with the token
    (get-token-uri (uint) (response (optional (string-utf8 256)) uint))
    ;; Owner of a specific token
    (get-owner (uint) (response (optional principal) uint))
    ;; Transfer token to a new principal
    (transfer (uint principal principal) (response bool uint))
  ))

;; Intellectual property registrations
(define-map property-registrations
  { record-id: uint }
  {
    name: (string-utf8 256),
    details: (string-utf8 1024),
    author: principal,
    established-at: uint,
    property-type: (string-ascii 32),     ;; "image", "music", "text", "code", "video", "design", etc.
    data-hash: (buff 64),        ;; Hash of the IP content
    state: (string-ascii 16),      ;; "registered", "disputed", "revoked"
    token-contract: (optional principal),  ;; Optional NFT contract for this IP
    token-id: (optional uint),        ;; Optional NFT ID within the contract
    open-domain: bool,            ;; Whether the work is in the public domain
    record-expiry: (optional uint)  ;; Optional block height when registration expires
  })

;; IP ownership shares (can be fractional)
(define-map property-ownership
  { record-id: uint, holder: principal }
  {
    ownership-percentage: uint,         ;; Out of 10000 (e.g., 5000 = 50%)
    obtained-at: uint,
    obtained-from: (optional principal)
  })

;; License templates
(define-map agreement-templates
  { template-ref: uint }
  {
    title: (string-utf8 64),
    details: (string-utf8 1024),
    author: principal,
    established-at: uint,
    permissions: (list 10 (string-ascii 32)),  ;; e.g., "reproduce", "distribute", "derivative", "commercial"
    fee-structure: (string-ascii 16),        ;; "one-time", "recurring", "usage-based", "free"
    standard-fee: uint,                          ;; Default fee amount
    standard-duration: (optional uint),          ;; Default duration in blocks
    assignable: bool,                         ;; Whether license can be transferred
    exclusive-available: bool,                ;; Whether exclusive licenses are available
    region-restricted: bool,                 ;; Whether license can be territory-restricted
    template-location: (string-utf8 256)             ;; URI to the full legal template
  })

;; Granted licenses
(define-map issued-licenses
  { agreement-id: uint }
  {
    record-id: uint,          ;; The IP being licensed
    template-ref: uint,              ;; The license template used
    grantor: principal,            ;; Entity granting the license
    grantee: principal,            ;; Entity receiving the license
    issued-at: uint,
    expires-at: (optional uint),
    payment-made: uint,
    region: (optional (string-ascii 64)),
    exclusive: bool,
    active: bool,
    usage-tracker: uint,            ;; Counter for usage-based licensing
    max-usage: (optional uint),     ;; Max allowed usage
    custom-terms: (optional (string-utf8 1024)),
    revoked: bool,
    revoked-reason: (optional (string-utf8 256))
  })

;; Usage logs for IP
(define-map property-usage-logs
  { record-id: uint, usage-ref: uint }
  {
    grantee: principal,
    agreement-id: (optional uint),
    usage-category: (string-ascii 32),
    service: (string-ascii 64),
    usage-proof: (buff 32),          ;; Hash of usage evidence
    logged-at: uint,
    income-generated: (optional uint),
    confirmed: bool,
    validator: (optional principal)
  })

;; Royalty recipients
(define-map payment-recipients
  { record-id: uint, beneficiary: principal }
  {
    ownership-percentage: uint,         ;; Out of 10000
    beneficiary-type: (string-ascii 16),  ;; "creator", "collaborator", "label", "publisher", etc.
    active: bool
  })

;; Royalty payments
(define-map payment-records
  { transaction-id: uint }
  {
    record-id: uint,
    agreement-id: (optional uint),
    sender: principal,
    sum: uint,
    logged-at: uint,
    usage-ref: (optional uint),
    transaction-type: (string-ascii 16),  ;; "license-fee", "royalty", "settlement"
    processed: bool
  })

;; Dispute records
(define-map property-disputes
  { case-id: uint }
  {
    record-id: uint,
    plaintiff: principal,
    submitted-at: uint,
    claim-reason: (string-utf8 256),
    proof-hash: (buff 32),
    state: (string-ascii 16),      ;; "pending", "resolved", "rejected", "withdrawn"
    outcome: (optional (string-utf8 256)),
    arbitrator: (optional principal),
    settled-at: (optional uint)
  })

;; Derivative works
(define-map derived-works
  { source-id: uint, derived-id: uint }
  {
    connection-type: (string-ascii 32),  ;; "adaptation", "translation", "remix", etc.
    authorized: bool,
    authorization-date: (optional uint),
    payment-percentage: uint        ;; How much goes back to original work
  })

;; Next available IDs
(define-data-var next-record-id uint u0)
(define-data-var next-template-ref uint u0)
(define-data-var next-agreement-id uint u0)
(define-data-var next-case-id uint u0)
(define-data-var next-transaction-id uint u0)
(define-map next-usage-ref { record-id: uint } { id: uint })

;; Protocol configuration
(define-data-var arbitration-address principal tx-sender)
(define-data-var system-fee-percentage uint u250)  ;; 2.5% of transactions
(define-data-var case-filing-fee uint u1000000)   ;; 1 STX

;; Validation functions
(define-private (validate-record-id (record-id uint))
  (if (< record-id (var-get next-record-id))
      (ok record-id)
      (err u"Invalid registration ID")))

(define-private (validate-utf8-256 (text (string-utf8 256)))
  (if (> (len text) u0)
      (ok text)
      (err u"Text cannot be empty")))

(define-private (validate-utf8-64 (text (string-utf8 64)))
  (if (> (len text) u0)
      (ok text)
      (err u"Text cannot be empty")))

(define-private (validate-utf8-1024 (text (string-utf8 1024)))
  (if (> (len text) u0)
      (ok text)
      (err u"Text cannot be empty")))

(define-private (validate-data-hash (data-hash (buff 64)))
  (if (> (len data-hash) u0)
      (ok data-hash)
      (err u"Content hash cannot be empty")))

(define-private (validate-template-ref (template-ref uint))
  (if (< template-ref (var-get next-template-ref))
      (ok template-ref)
      (err u"Invalid template ID")))

(define-private (validate-agreement-id (agreement-id uint))
  (if (< agreement-id (var-get next-agreement-id))
      (ok agreement-id)
      (err u"Invalid license ID")))

(define-private (validate-case-id (case-id uint))
  (if (< case-id (var-get next-case-id))
      (ok case-id)
      (err u"Invalid dispute ID")))

(define-private (validate-usage-ref (record-id uint) (usage-ref uint))
  (match (map-get? next-usage-ref { record-id: record-id })
    counter (if (< usage-ref (get id counter))
               (ok usage-ref)
               (err u"Invalid usage ID"))
    (err u"Registration ID not found")))

(define-private (validate-connection-type (connection-type (string-ascii 32)))
  (if (or (is-eq connection-type "adaptation")
          (or (is-eq connection-type "translation")
              (or (is-eq connection-type "remix")
                  (is-eq connection-type "derivative"))))
      (ok connection-type)
      (err u"Invalid relationship type")))

(define-private (validate-usage-category (usage-category (string-ascii 32)))
  (if (or (is-eq usage-category "online-display")
          (or (is-eq usage-category "broadcast")
              (or (is-eq usage-category "print")
                  (or (is-eq usage-category "merchandise")
                      (is-eq usage-category "performance")))))
      (ok usage-category)
      (err u"Invalid usage type")))

(define-private (validate-beneficiary-type (beneficiary-type (string-ascii 16)))
  (if (or (is-eq beneficiary-type "creator")
          (or (is-eq beneficiary-type "collaborator")
              (or (is-eq beneficiary-type "label")
                  (or (is-eq beneficiary-type "publisher")
                      (is-eq beneficiary-type "distributor")))))
      (ok beneficiary-type)
      (err u"Invalid recipient type")))

(define-private (validate-transaction-type (transaction-type (string-ascii 16)))
  (if (or (is-eq transaction-type "license-fee")
          (or (is-eq transaction-type "royalty")
              (is-eq transaction-type "settlement")))
      (ok transaction-type)
      (err u"Invalid payment type")))

;; Register new intellectual property
(define-public (register-ip
                (name (string-utf8 256))
                (details (string-utf8 1024))
                (property-type (string-ascii 32))
                (data-hash (buff 64))
                (open-domain bool)
                (record-expiry (optional uint)))
  (let
    ((validated-name-resp (validate-utf8-256 name))
     (validated-details-resp (validate-utf8-1024 details))
     (validated-data-hash-resp (validate-data-hash data-hash))
     (record-id (var-get next-record-id)))
    
    ;; Validate parameters
    (asserts! (is-valid-property-type property-type) (err u"Invalid IP type"))
    (asserts! (is-ok validated-name-resp) (err (unwrap-err! validated-name-resp (err u"Title validation failed"))))
    (asserts! (is-ok validated-details-resp) (err (unwrap-err! validated-details-resp (err u"Description validation failed"))))
    (asserts! (is-ok validated-data-hash-resp) (err (unwrap-err! validated-data-hash-resp (err u"Content hash validation failed"))))
    
    ;; Create the registration
    (map-set property-registrations
      { record-id: record-id }
      {
        name: (unwrap-panic validated-name-resp),
        details: (unwrap-panic validated-details-resp),
        author: tx-sender,
        established-at: block-height,
        property-type: property-type,
        data-hash: (unwrap-panic validated-data-hash-resp),
        state: "registered",
        token-contract: none,
        token-id: none,
        open-domain: open-domain,
        record-expiry: record-expiry
      }
    )
    
    ;; Set initial ownership
    (map-set property-ownership
      { record-id: record-id, holder: tx-sender }
      {
        ownership-percentage: u10000,     ;; 100%
        obtained-at: block-height,
        obtained-from: none
      }
    )
    
    ;; Initialize usage counter
    (map-set next-usage-ref
      { record-id: record-id }
      { id: u0 }
    )
    
    ;; Increment registration ID counter
    (var-set next-record-id (+ record-id u1))
    
    (ok record-id)
  ))

;; Check if IP type is valid
(define-private (is-valid-property-type (property-type (string-ascii 32)))
  (or (is-eq property-type "image")
      (or (is-eq property-type "music")
          (or (is-eq property-type "text")
              (or (is-eq property-type "code")
                  (or (is-eq property-type "video")
                      (is-eq property-type "design")))))))

;; Link an NFT to an IP registration
(define-public (link-nft-to-ip
                (record-id uint)
                (token-contract principal)
                (token-id uint))
  (let
    ((validated-id-resp (validate-record-id record-id)))
    
    ;; Validate registration ID is valid
    (asserts! (is-ok validated-id-resp)
              (err (unwrap-err! validated-id-resp (err u"Invalid registration ID"))))
    
    (let ((validated-id (unwrap-panic validated-id-resp)))
      ;; Get the registration
      (let ((registration (unwrap! (map-get? property-registrations { record-id: validated-id })
                                  (err u"Registration not found"))))
        ;; Validate
        (asserts! (is-eq tx-sender (get author registration))
                  (err u"Only creator can link NFT"))
        (asserts! (is-eq (get state registration) "registered")
                  (err u"Registration not in valid state"))
        
        ;; TODO: In a real implementation, verify NFT ownership
        
        ;; Update registration with NFT info
        (map-set property-registrations
          { record-id: validated-id }
          (merge registration 
            {
              token-contract: (some token-contract),
              token-id: (some token-id)
            }
          )
        )
        
        (ok true)
      )
    )
  ))

;; Create a license template
(define-public (create-license-template
                (title (string-utf8 64))
                (details (string-utf8 1024))
                (permissions (list 10 (string-ascii 32)))
                (fee-structure (string-ascii 16))
                (standard-fee uint)
                (standard-duration (optional uint))
                (assignable bool)
                (exclusive-available bool)
                (region-restricted bool)
                (template-location (string-utf8 256)))
  (let
    ((validated-title-resp (validate-utf8-64 title))
     (validated-details-resp (validate-utf8-1024 details))
     (template-ref (var-get next-template-ref)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-title-resp)
              (err (unwrap-err! validated-title-resp (err u"Name validation failed"))))
    (asserts! (is-ok validated-details-resp)
              (err (unwrap-err! validated-details-resp (err u"Description validation failed"))))
    (asserts! (is-valid-fee-structure fee-structure) (err u"Invalid fee type"))
    (asserts! (> (len permissions) u0) (err u"Must provide at least one usage right"))
    
    (let
      ((validated-title (unwrap-panic validated-title-resp))
       (validated-details (unwrap-panic validated-details-resp)))
      
      ;; Create the template
      (map-set agreement-templates
        { template-ref: template-ref }
        {
          title: validated-title,
          details: validated-details,
          author: tx-sender,
          established-at: block-height,
          permissions: permissions,
          fee-structure: fee-structure,
          standard-fee: standard-fee,
          standard-duration: standard-duration,
          assignable: assignable,
          exclusive-available: exclusive-available,
          region-restricted: region-restricted,
          template-location: template-location
        }
      )
      
      ;; Increment template ID counter
      (var-set next-template-ref (+ template-ref u1))
      
      (ok template-ref)
    )
  ))

;; Check if fee type is valid
(define-private (is-valid-fee-structure (fee-structure (string-ascii 16)))
  (or (is-eq fee-structure "one-time")
      (or (is-eq fee-structure "recurring")
          (or (is-eq fee-structure "usage-based")
              (is-eq fee-structure "free")))))

;; Grant a license to use IP - split into free and paid versions
;; This version is for free licenses (fee = 0)
(define-public (grant-free-license
                (record-id uint)
                (template-ref uint)
                (grantee principal)
                (duration (optional uint))
                (region (optional (string-ascii 64)))
                (exclusive bool)
                (max-usage (optional uint))
                (custom-terms (optional (string-utf8 1024))))
  (let
    ((validated-record-id-resp (validate-record-id record-id))
     (validated-template-ref-resp (validate-template-ref template-ref)))
    
    ;; Check validation results
    (asserts! (is-ok validated-record-id-resp)
              (err (unwrap-err! validated-record-id-resp (err u"Invalid registration ID"))))
    (asserts! (is-ok validated-template-ref-resp)
              (err (unwrap-err! validated-template-ref-resp (err u"Invalid template ID"))))
    
    (let ((validated-record-id (unwrap-panic validated-record-id-resp))
          (validated-template-ref (unwrap-panic validated-template-ref-resp)))
      
      ;; Get registration and template records
      (let ((registration (unwrap! (map-get? property-registrations { record-id: validated-record-id })
                                  (err u"Registration not found")))
            (template (unwrap! (map-get? agreement-templates { template-ref: validated-template-ref })
                              (err u"Template not found")))
            (ownership (unwrap! (map-get? property-ownership
                                { record-id: validated-record-id, holder: tx-sender })
                              (err u"Not an owner of this IP")))
            (agreement-id (var-get next-agreement-id)))
        
        ;; Validate
        (asserts! (is-eq (get state registration) "registered")
                  (err u"Registration not in valid state"))
        (asserts! (not (get open-domain registration))
                  (err u"Public domain works don't require licenses"))
        (asserts! (or (not exclusive) (get exclusive-available template))
                  (err u"Exclusive license not available for this template"))
        (asserts! (or (is-none region) (get region-restricted template))
                  (err u"Territory restrictions not available for this template"))
        
        ;; Calculate expiration if duration provided
        (let ((expiry (if (is-some duration)
                          (some (+ block-height (unwrap-panic duration)))
                          (get standard-duration template))))
          
          ;; Create the license grant
          (map-set issued-licenses
            { agreement-id: agreement-id }
            {
              record-id: validated-record-id,
              template-ref: validated-template-ref,
              grantor: tx-sender,
              grantee: grantee,
              issued-at: block-height,
              expires-at: expiry,
              payment-made: u0,  ;; Free license
              region: region,
              exclusive: exclusive,
              active: true,
              usage-tracker: u0,
              max-usage: max-usage,
              custom-terms: custom-terms,
              revoked: false,
              revoked-reason: none
            }
          )
          
          ;; Increment license ID counter
          (var-set next-agreement-id (+ agreement-id u1))
          
          (ok agreement-id)
        )
      )
    )
  ))

;; Grant a license with payment
(define-public (grant-paid-license
                (record-id uint)
                (template-ref uint)
                (grantee principal)
                (fee uint)  ;; Must be > 0
                (duration (optional uint))
                (region (optional (string-ascii 64)))
                (exclusive bool)
                (max-usage (optional uint))
                (custom-terms (optional (string-utf8 1024))))
  (let
    ((validated-record-id-resp (validate-record-id record-id))
     (validated-template-ref-resp (validate-template-ref template-ref)))
    
    ;; Check validation results
    (asserts! (is-ok validated-record-id-resp)
              (err (unwrap-err! validated-record-id-resp (err u"Invalid registration ID"))))
    (asserts! (is-ok validated-template-ref-resp)
              (err (unwrap-err! validated-template-ref-resp (err u"Invalid template ID"))))
    
    (let ((validated-record-id (unwrap-panic validated-record-id-resp))
          (validated-template-ref (unwrap-panic validated-template-ref-resp)))
      
      ;; Get registration and template records
      (let ((registration (unwrap! (map-get? property-registrations { record-id: validated-record-id })
                                  (err u"Registration not found")))
            (template (unwrap! (map-get? agreement-templates { template-ref: validated-template-ref })
                              (err u"Template not found")))
            (ownership (unwrap! (map-get? property-ownership
                                { record-id: validated-record-id, holder: tx-sender })
                              (err u"Not an owner of this IP")))
            (agreement-id (var-get next-agreement-id))
            (system-fee (/ (* fee (var-get system-fee-percentage)) u10000)))
        
        ;; Validate
        (asserts! (is-eq (get state registration) "registered")
                  (err u"Registration not in valid state"))
        (asserts! (not (get open-domain registration))
                  (err u"Public domain works don't require licenses"))
        (asserts! (or (not exclusive) (get exclusive-available template))
                  (err u"Exclusive license not available for this template"))
        (asserts! (or (is-none region) (get region-restricted template))
                  (err u"Territory restrictions not available for this template"))
        (asserts! (> fee u0) (err u"Fee must be greater than 0"))
        
        ;; Transfer fee from licensee
        (asserts! (is-ok (stx-transfer? fee grantee (as-contract tx-sender)))
                  (err u"License fee transfer failed"))
        
        ;; Transfer protocol fee
        (asserts! (is-ok (as-contract (stx-transfer? system-fee tx-sender (var-get arbitration-address))))
                 (err u"Protocol fee transfer failed"))
        
        ;; Calculate expiration if duration provided
        (let ((expiry (if (is-some duration)
                          (some (+ block-height (unwrap-panic duration)))
                          (get standard-duration template))))
          
          ;; Create the license grant
          (map-set issued-licenses
            { agreement-id: agreement-id }
            {
              record-id: validated-record-id,
              template-ref: validated-template-ref,
              grantor: tx-sender,
              grantee: grantee,
              issued-at: block-height,
              expires-at: expiry,
              payment-made: fee,
              region: region,
              exclusive: exclusive,
              active: true,
              usage-tracker: u0,
              max-usage: max-usage,
              custom-terms: custom-terms,
              revoked: false,
              revoked-reason: none
            }
          )
          
          ;; Record payment
          (let ((transaction-id (var-get next-transaction-id)))
            ;; Create payment record
            (map-set payment-records
              { transaction-id: transaction-id }
              {
                record-id: validated-record-id,
                agreement-id: (some agreement-id),
                sender: grantee,
                sum: fee,
                logged-at: block-height,
                usage-ref: none,
                transaction-type: "license-fee",
                processed: true  ;; Simplified for this example
              }
            )
            
            ;; Increment payment ID counter
            (var-set next-transaction-id (+ transaction-id u1))
          )
          
          ;; Increment license ID counter
          (var-set next-agreement-id (+ agreement-id u1))
          
          (ok agreement-id)
        )
      )
    )
  ))

;; Record IP usage
(define-public (record-ip-usage
                (record-id uint)
                (agreement-id (optional uint))
                (usage-category (string-ascii 32))
                (service (string-ascii 64))
                (usage-proof (buff 32))
                (income-generated (optional uint)))
  (let
    ((validated-record-id-resp (validate-record-id record-id))
     (validated-usage-category-resp (validate-usage-category usage-category)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-record-id-resp)
              (err (unwrap-err! validated-record-id-resp (err u"Invalid registration ID"))))
    (asserts! (is-ok validated-usage-category-resp)
              (err (unwrap-err! validated-usage-category-resp (err u"Invalid usage type"))))
    
    (let ((validated-record-id (unwrap-panic validated-record-id-resp))
          (validated-usage-category (unwrap-panic validated-usage-category-resp)))
      
      ;; Get registration and usage counter
      (let ((registration (unwrap! (map-get? property-registrations
                                  { record-id: validated-record-id })
                                 (err u"Registration not found")))
            (usage-counter (unwrap! (map-get? next-usage-ref
                                   { record-id: validated-record-id })
                                    (err u"Counter not found")))
            (usage-ref (get id usage-counter)))
        
        ;; Validate license if provided
        (if (is-some agreement-id)
            (let ((agreement-id-value (unwrap-panic agreement-id))
                  (validated-agreement-id-resp (validate-agreement-id (unwrap-panic agreement-id))))
              
              (asserts! (is-ok validated-agreement-id-resp)
                        (err (unwrap-err! validated-agreement-id-resp (err u"Invalid license ID"))))
              
              (let ((validated-agreement-id (unwrap-panic validated-agreement-id-resp))
                    (license (unwrap! (map-get? issued-licenses
                                      { agreement-id: validated-agreement-id })
                                    (err u"License not found"))))
                ;; Check license validity
                (asserts! (and (is-eq (get record-id license) validated-record-id)
                              (is-eq (get grantee license) tx-sender))
                          (err u"Invalid license for this usage"))
                (asserts! (get active license) (err u"License not active"))
                (asserts! (not (get revoked license)) (err u"License revoked"))
                
                ;; Check license expiration
                (if (is-some (get expires-at license))
                    (asserts! (< block-height (unwrap-panic (get expires-at license)))
                              (err u"License expired"))
                    true)
                
                ;; Check usage limits
                (if (is-some (get max-usage license))
                    (asserts! (< (get usage-tracker license) (unwrap-panic (get max-usage license)))
                              (err u"Usage limit exceeded"))
                    true)
                
                ;; Update usage counter for license
                (map-set issued-licenses
                  { agreement-id: validated-agreement-id }
                  (merge license { usage-tracker: (+ (get usage-tracker license) u1) })
                )
              )
            )
            ;; If no license provided, ensure the work is public domain
            (asserts! (get open-domain registration) (err u"Non-public domain works require a license"))
        )
        
        ;; Create the usage record
        (map-set property-usage-logs
          { record-id: validated-record-id, usage-ref: usage-ref }
          {
            grantee: tx-sender,
            agreement-id: agreement-id,
            usage-category: validated-usage-category,
            service: service,
            usage-proof: usage-proof,
            logged-at: block-height,
            income-generated: income-generated,
            confirmed: false,
            validator: none
          }
        )
        
        ;; Increment usage counter
        (map-set next-usage-ref
          { record-id: validated-record-id }
          { id: (+ usage-ref u1) }
        )
        
        ;; If revenue was generated, process royalty payment
        (if (and (is-some income-generated) (> (unwrap-panic income-generated) u0))
            (record-usage-royalty validated-record-id usage-ref (unwrap-panic income-generated))
            (ok usage-ref))
      )
    )
  ))

;; Record royalty from usage revenue
(define-public (record-usage-royalty (record-id uint) (usage-ref uint) (revenue uint))
  (let
    ((validated-record-id-resp (validate-record-id record-id)))
    
    ;; Validate registration ID
    (asserts! (is-ok validated-record-id-resp)
              (err (unwrap-err! validated-record-id-resp (err u"Invalid registration ID"))))
    
    (let ((validated-record-id (unwrap-panic validated-record-id-resp)))
      ;; Validate usage ID with the unwrapped registration ID
      (let ((validated-usage-ref-resp (validate-usage-ref validated-record-id usage-ref)))
        
        ;; Check if usage ID is valid
        (asserts! (is-ok validated-usage-ref-resp)
                  (err (unwrap-err! validated-usage-ref-resp (err u"Invalid usage ID"))))
        
        (let ((validated-usage-ref (unwrap-panic validated-usage-ref-resp))
              (standard-payment-rate u1000)  ;; 10% standard rate
              (payment-amount (/ (* revenue standard-payment-rate) u10000))
              (transaction-id (var-get next-transaction-id)))
          
          ;; Create payment record
          (map-set payment-records
            { transaction-id: transaction-id }
            {
              record-id: validated-record-id,
              agreement-id: none,
              sender: tx-sender,
              sum: payment-amount,
              logged-at: block-height,
              usage-ref: (some validated-usage-ref),
              transaction-type: "royalty",
              processed: false
            }
          )
          
          ;; Increment payment ID counter
          (var-set next-transaction-id (+ transaction-id u1))
          
          ;; Transfer royalty payment
          (asserts! (is-ok (stx-transfer? payment-amount tx-sender (as-contract tx-sender)))
                    (err u"Royalty payment transfer failed"))
          
          ;; Mark as distributed
          (map-set payment-records
            { transaction-id: transaction-id }
            (merge (unwrap-panic (map-get? payment-records { transaction-id: transaction-id }))
              { processed: true })
          )
          
          (ok transaction-id)
        )
      )
    )
  ))

;; Verify IP usage
(define-public (verify-ip-usage (record-id uint) (usage-ref uint))
  (let
    ((validated-record-id-resp (validate-record-id record-id)))
    
    ;; Validate registration ID
    (asserts! (is-ok validated-record-id-resp)
              (err (unwrap-err! validated-record-id-resp (err u"Invalid registration ID"))))
    
    (let ((validated-record-id (unwrap-panic validated-record-id-resp)))
      ;; Validate usage ID with the unwrapped registration ID
      (let ((validated-usage-ref-resp (validate-usage-ref validated-record-id usage-ref)))
        
        ;; Check if usage ID is valid
        (asserts! (is-ok validated-usage-ref-resp)
                  (err (unwrap-err! validated-usage-ref-resp (err u"Invalid usage ID"))))
        
        (let ((validated-usage-ref (unwrap-panic validated-usage-ref-resp))
              (registration (unwrap! (map-get? property-registrations
                                      { record-id: validated-record-id })
                                     (err u"Registration not found")))
              (usage (unwrap! (map-get? property-usage-logs
                              { record-id: validated-record-id, usage-ref: validated-usage-ref })
                            (err u"Usage not found"))))
          
          ;; Validate
          (asserts! (or (is-eq tx-sender (get author registration))
                       (is-property-holder validated-record-id tx-sender))
                    (err u"Not authorized to verify usage"))
          
          ;; Update usage verification
          (map-set property-usage-logs
            { record-id: validated-record-id, usage-ref: validated-usage-ref }
            (merge usage { 
              confirmed: true,
              validator: (some tx-sender)
            })
          )
          
          (ok true)
        )
      )
    )
  ))

;; Check if principal is an IP owner
(define-private (is-property-holder (record-id uint) (account principal))
  (is-some (map-get? property-ownership { record-id: record-id, holder: account })))

;; Transfer IP ownership shares
(define-public (transfer-ip-shares
                (record-id uint)
                (beneficiary principal)
                (ownership-percentage uint))
  (let
    ((validated-record-id-resp (validate-record-id record-id)))
    
    ;; Validate registration ID
    (asserts! (is-ok validated-record-id-resp)
              (err (unwrap-err! validated-record-id-resp (err u"Invalid registration ID"))))
    
    (let ((validated-record-id (unwrap-panic validated-record-id-resp))
          (registration (unwrap! (map-get? property-registrations
                                { record-id: (unwrap-panic validated-record-id-resp) })
                               (err u"Registration not found")))
          (sender-ownership (unwrap! (map-get? property-ownership
                                    { record-id: (unwrap-panic validated-record-id-resp), holder: tx-sender })
                                  (err u"No ownership found")))
          (beneficiary-ownership (map-get? property-ownership
                              { record-id: (unwrap-panic validated-record-id-resp), holder: beneficiary })))
      
      ;; Validate
      (asserts! (is-eq (get state registration) "registered")
                (err u"Registration not in valid state"))
      (asserts! (<= ownership-percentage (get ownership-percentage sender-ownership))
                (err u"Insufficient ownership shares"))
      (asserts! (> ownership-percentage u0)
                (err u"Share percentage must be greater than zero"))
      
      ;; Update sender's ownership
      (map-set property-ownership
        { record-id: validated-record-id, holder: tx-sender }
        (merge sender-ownership 
          { ownership-percentage: (- (get ownership-percentage sender-ownership) ownership-percentage) }
        )
      )
      
      ;; Update or create recipient's ownership
      (if (is-some beneficiary-ownership)
          (map-set property-ownership
            { record-id: validated-record-id, holder: beneficiary }
            (merge (unwrap-panic beneficiary-ownership)
              { 
                ownership-percentage: (+ (get ownership-percentage (unwrap-panic beneficiary-ownership))
                                   ownership-percentage),
                obtained-at: block-height,
                obtained-from: (some tx-sender)
              }
            )
          )
          (map-set property-ownership
            { record-id: validated-record-id, holder: beneficiary }
            {
              ownership-percentage: ownership-percentage,
              obtained-at: block-height,
              obtained-from: (some tx-sender)
            }
          )
      )
      
      (ok true)
    )
  ))