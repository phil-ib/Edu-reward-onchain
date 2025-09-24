;; Educhain Logic - Educational Achievement System
;; Professional smart contract for managing educational achievements and certifications

;; === CONSTANTS ===
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-ACHIEVEMENT-NAME-LENGTH u100)
(define-constant MAX-DESCRIPTION-LENGTH u500)
(define-constant MAX-CATEGORY-LENGTH u50)
(define-constant MIN-REWARD-AMOUNT u1000)
(define-constant MAX-REWARD-AMOUNT u1000000)
(define-constant MAX-ACHIEVEMENTS-PER-USER u100)
(define-constant MAX-CERTIFICATIONS-PER-USER u50)

;; === ERROR CODES ===
(define-constant ERR-UNAUTHORIZED (err u1001))
(define-constant ERR-INVALID-INPUT (err u1002))
(define-constant ERR-ACHIEVEMENT-NOT-FOUND (err u1003))
(define-constant ERR-USER-NOT-FOUND (err u1004))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u1005))
(define-constant ERR-INSUFFICIENT-BALANCE (err u1006))
(define-constant ERR-LIMIT-EXCEEDED (err u1007))
(define-constant ERR-CERTIFICATION-NOT-FOUND (err u1008))

;; === DATA STRUCTURES ===

;; Achievement definitions
(define-map achievement-definitions uint {
  name: (string-ascii 100),
  description: (string-ascii 500),
  category: (string-ascii 50),
  reward-amount: uint,
  issuer: principal,
  active: bool,
  created-at: uint
})

;; User achievements
(define-map user-achievements {user: principal, achievement-id: uint} {
  earned-at: uint,
  claimed: bool,
  issuer: principal
})

;; User profiles
(define-map user-profiles principal {
  total-achievements: uint,
  total-rewards-claimed: uint,
  total-points: uint,
  joined-at: uint,
  last-activity: uint
})

;; Certifications
(define-map certifications uint {
  name: (string-ascii 100),
  description: (string-ascii 500),
  required-achievements-count: uint,
  issuer: principal,
  active: bool,
  created-at: uint
})

;; User certifications
(define-map user-certifications {user: principal, certification-id: uint} {
  earned-at: uint,
  issuer: principal
})

;; Authorized issuers
(define-map authorized-issuers principal {
  name: (string-ascii 100),
  description: (string-ascii 500),
  active: bool,
  registered-at: uint
})

;; === STATE VARIABLES ===
(define-data-var total-achievements uint u0)
(define-data-var total-certifications uint u0)
(define-data-var total-users uint u0)
(define-data-var contract-balance uint u0)
(define-data-var contract-paused bool false)

;; === PRIVATE HELPERS ===

(define-private (is-contract-paused)
  (var-get contract-paused))

(define-private (is-owner)
  (is-eq tx-sender CONTRACT-OWNER))

(define-private (is-authorized-issuer (issuer principal))
  (match (map-get? authorized-issuers issuer)
    issuer-data (get active issuer-data)
    false))

(define-private (validate-string-length (input (string-ascii 500)) (max-length uint))
  (<= (len input) max-length))

(define-private (validate-reward-amount (amount uint))
  (and (>= amount MIN-REWARD-AMOUNT) (<= amount MAX-REWARD-AMOUNT)))

(define-private (get-current-time)
  block-height)

(define-private (create-or-update-user-profile (user principal))
  (let ((current-time (get-current-time)))
    (match (map-get? user-profiles user)
      existing-profile 
      (map-set user-profiles user 
        (merge existing-profile {last-activity: current-time}))
      (begin
        (map-set user-profiles user {
          total-achievements: u0,
          total-rewards-claimed: u0,
          total-points: u0,
          joined-at: current-time,
          last-activity: current-time
        })
        (var-set total-users (+ (var-get total-users) u1))))))

(define-private (user-has-achievement (user principal) (achievement-id uint))
  (is-some (map-get? user-achievements {user: user, achievement-id: achievement-id})))

(define-private (user-has-certification (user principal) (certification-id uint))
  (is-some (map-get? user-certifications {user: user, certification-id: certification-id})))

(define-private (get-user-achievement (user principal) (achievement-id uint))
  (map-get? user-achievements {user: user, achievement-id: achievement-id}))

(define-private (get-achievement-definition (achievement-id uint))
  (map-get? achievement-definitions achievement-id))

(define-private (get-certification-definition (certification-id uint))
  (map-get? certifications certification-id))

(define-private (get-user-achievement-count (user principal))
  (match (map-get? user-profiles user)
    profile (get total-achievements profile)
    u0))

(define-private (user-meets-certification-requirements (user principal) (required-achievements-count uint))
  (>= (get-user-achievement-count user) required-achievements-count))

(define-private (validate-achievement-input 
  (name (string-ascii 100))
  (description (string-ascii 500))
  (category (string-ascii 50))
  (reward-amount uint))
  (and
    (validate-string-length name u100)
    (validate-string-length description u500)
    (validate-string-length category u50)
    (validate-reward-amount reward-amount)
    (not (is-eq name ""))
    (not (is-eq category ""))))

(define-private (validate-certification-input
  (name (string-ascii 100))
  (description (string-ascii 500))
  (required-achievements-count uint))
  (and
    (validate-string-length name u100)
    (validate-string-length description u500)
    (not (is-eq name ""))
    (> required-achievements-count u0)))

(define-private (user-achievement-limit-reached (user principal))
  (match (map-get? user-profiles user)
    profile (>= (get total-achievements profile) MAX-ACHIEVEMENTS-PER-USER)
    false))

(define-private (user-certification-limit-reached (user principal))
  (let ((certification-count u0))
    (>= certification-count MAX-CERTIFICATIONS-PER-USER)))

;; === PUBLIC FUNCTIONS ===

;; Register a new authorized issuer (owner only)
(define-public (register-issuer 
  (issuer principal)
  (name (string-ascii 100))
  (description (string-ascii 500)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (not (is-contract-paused)) ERR-INVALID-INPUT)
    (asserts! (validate-string-length name u100) ERR-INVALID-INPUT)
    (asserts! (validate-string-length description u500) ERR-INVALID-INPUT)
    (asserts! (not (is-eq name "")) ERR-INVALID-INPUT)
    (map-set authorized-issuers issuer {
      name: name,
      description: description,
      active: true,
      registered-at: (get-current-time)
    })
    (ok true)))

;; Deactivate an authorized issuer (owner only)
(define-public (deactivate-issuer (issuer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (not (is-contract-paused)) ERR-INVALID-INPUT)
    (match (map-get? authorized-issuers issuer)
      issuer-data 
      (begin
        (map-set authorized-issuers issuer
          (merge issuer-data {active: false}))
        (ok true))
      ERR-INVALID-INPUT)))

;; Create a new achievement (authorized issuers only)
(define-public (create-achievement
  (name (string-ascii 100))
  (description (string-ascii 500))
  (category (string-ascii 50))
  (reward-amount uint))
  (begin
    (asserts! (not (is-contract-paused)) ERR-INVALID-INPUT)
    (asserts! (is-authorized-issuer tx-sender) ERR-UNAUTHORIZED)
    (asserts! (validate-achievement-input name description category reward-amount) ERR-INVALID-INPUT)
    (let ((new-achievement-id (+ (var-get total-achievements) u1)))
      (map-set achievement-definitions new-achievement-id {
        name: name,
        description: description,
        category: category,
        reward-amount: reward-amount,
        issuer: tx-sender,
        active: true,
        created-at: (get-current-time)
      })
      (var-set total-achievements new-achievement-id)
      (ok new-achievement-id))))

;; Award an achievement to a user (authorized issuers only)
(define-public (award-achievement (user principal) (achievement-id uint))
  (begin
    (asserts! (not (is-contract-paused)) ERR-INVALID-INPUT)
    (asserts! (is-authorized-issuer tx-sender) ERR-UNAUTHORIZED)
    (asserts! (not (user-has-achievement user achievement-id)) ERR-INVALID-INPUT)
    (asserts! (not (user-achievement-limit-reached user)) ERR-LIMIT-EXCEEDED)
    (match (get-achievement-definition achievement-id)
      achievement-def 
      (begin
        (asserts! (get active achievement-def) ERR-ACHIEVEMENT-NOT-FOUND)
        (create-or-update-user-profile user)
        (map-set user-achievements {user: user, achievement-id: achievement-id} {
          earned-at: (get-current-time),
          claimed: false,
          issuer: tx-sender
        })
        (match (map-get? user-profiles user)
          profile 
          (map-set user-profiles user 
            (merge profile {
              total-achievements: (+ (get total-achievements profile) u1),
              total-points: (+ (get total-points profile) (get reward-amount achievement-def))
            }))
          true)
        (ok true))
      ERR-ACHIEVEMENT-NOT-FOUND)))

;; Deactivate an achievement (issuer or owner only)
(define-public (deactivate-achievement (achievement-id uint))
  (begin
    (asserts! (not (is-contract-paused)) ERR-INVALID-INPUT)
    (match (get-achievement-definition achievement-id)
      achievement-def 
      (begin
        (asserts! (or (is-owner) (is-eq tx-sender (get issuer achievement-def))) ERR-UNAUTHORIZED)
        (map-set achievement-definitions achievement-id
          (merge achievement-def {active: false}))
        (ok true))
      ERR-ACHIEVEMENT-NOT-FOUND)))

;; Create a new certification (authorized issuers only)
(define-public (create-certification
  (name (string-ascii 100))
  (description (string-ascii 500))
  (required-achievements-count uint))
  (begin
    (asserts! (not (is-contract-paused)) ERR-INVALID-INPUT)
    (asserts! (is-authorized-issuer tx-sender) ERR-UNAUTHORIZED)
    (asserts! (validate-certification-input name description required-achievements-count) ERR-INVALID-INPUT)
    (let ((new-certification-id (+ (var-get total-certifications) u1)))
      (map-set certifications new-certification-id {
        name: name,
        description: description,
        required-achievements-count: required-achievements-count,
        issuer: tx-sender,
        active: true,
        created-at: (get-current-time)
      })
      (var-set total-certifications new-certification-id)
      (ok new-certification-id))))