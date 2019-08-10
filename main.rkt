#lang racket

(require racket/tcp
         racket/port
         racket/generic)

(define DEBUG? false)
(define (mqtt-debug bool)
  (set! DEBUG? bool))

(define-syntax-rule (debug fmt-str args ...)
  (when DEBUG?
    (printf fmt-str args ...)))

(define PROTOCOL-VERSION 'v3.1.1)
(define RESERVED-ZERO 0)
(define RESERVED-ONE 1)

;; Message types
(define CONNECT     #x10)
(define CONNACK     #x20)
(define PUBLISH     #x30)
(define PUBACK      #x40)
(define PUBREC      #x50)
(define PUBREL      #x60)
(define PUBCOMP     #x70)
(define SUBSCRIBE   #x80)
(define SUBACK      #x90)
(define UNSUBSCRIBE #xA0)
(define UNSUBACK    #xB0)
(define PINGREQ     #xC0)
(define PINGRESP    #xD0)
(define DISCONNECT  #xE0)

;; CONNACK codes
(define CONNACK_ACCEPTED                      0)
(define CONNACK_REFUSED_PROTOCOL_VERSION      1)
(define CONNACK_REFUSED_IDENTIFIER_REJECTED   2)
(define CONNACK_REFUSED_SERVER_UNAVAILABLE    3)
(define CONNACK_REFUSED_BAD_USERNAME_PASSWORD 4)
(define CONNACK_REFUSED_NOT_AUTHORIZED        5)

;; Connection state
(define mqtt_cs_new           0)
(define mqtt_cs_connected     1)
(define mqtt_cs_disconnecting 2)
(define mqtt_cs_connect_async 3)

;; Message state
(define mqtt_ms_invalid          0)
(define mqtt_ms_publish          1)
(define mqtt_ms_wait_for_puback  2)
(define mqtt_ms_wait_for_pubrec  3)
(define mqtt_ms_resend_pubrel    4)
(define mqtt_ms_wait_for_pubrel  5)
(define mqtt_ms_resend_pubcomp   6)
(define mqtt_ms_wait_for_pubcomp 7)
(define mqtt_ms_send_pubrec      8)
(define mqtt_ms_queued           9)

;; Error values
(define MQTT_ERR_AGAIN         -1)
(define MQTT_ERR_SUCCESS        0)
(define MQTT_ERR_NOMEM          1)
(define MQTT_ERR_PROTOCOL       2)
(define MQTT_ERR_INVAL          3)
(define MQTT_ERR_NO_CONN        4)
(define MQTT_ERR_CONN_REFUSED   5)
(define MQTT_ERR_NOT_FOUND      6)
(define MQTT_ERR_CONN_LOST      7)
(define MQTT_ERR_TLS            8)
(define MQTT_ERR_PAYLOAD_SIZE   9)
(define MQTT_ERR_NOT_SUPPORTED  10)
(define MQTT_ERR_AUTH           11)
(define MQTT_ERR_ACL_DENIED     12)
(define MQTT_ERR_UNKNOWN        13)
(define MQTT_ERR_ERRNO          14)
(define MQTT_ERR_QUEUE_SIZE     15)

(define MQTT_CLIENT  0)
(define MQTT_BRIDGE  1)

(define (error-string mqtt_err)
  (cond
    [(eqv? MQTT_ERR_SUCCESS       mqtt_err)
     "No error."]
    [(eqv? MQTT_ERR_NOMEM         mqtt_err)
     "Out of memory."]
    [(eqv? MQTT_ERR_PROTOCOL      mqtt_err)
     "A network protocol error occurred when communicating with the broker."]
    [(eqv? MQTT_ERR_INVAL         mqtt_err)
     "Invalid function arguments provided."]
    [(eqv? MQTT_ERR_NO_CONN       mqtt_err)
     "The client is not currently connected."]
    [(eqv? MQTT_ERR_CONN_REFUSED  mqtt_err)
     "The connection was refused."]
    [(eqv? MQTT_ERR_NOT_FOUND     mqtt_err)
     "Message not found (internal error)."]
    [(eqv? MQTT_ERR_CONN_LOST     mqtt_err)
     "The connection was lost."]
    [(eqv? MQTT_ERR_TLS           mqtt_err)
     "A TLS error occurred."]
    [(eqv? MQTT_ERR_PAYLOAD_SIZE  mqtt_err)
     "Payload too large."]
    [(eqv? MQTT_ERR_NOT_SUPPORTED mqtt_err)
     "This feature is not supported."]
    [(eqv? MQTT_ERR_AUTH          mqtt_err)
     "Authorisation failed."]
    [(eqv? MQTT_ERR_ACL_DENIED    mqtt_err)
     "Access denied by ACL."]
    [(eqv? MQTT_ERR_UNKNOWN       mqtt_err)
     "Unknown error."]
    [(eqv? MQTT_ERR_ERRNO         mqtt_err)
     "Error defined by errno."]
    [else "Unknown error."]))

(define (connack-string connack_code)
  (cond
    [(eqv? CONNACK_ACCEPTED                      connack_code)
     "Connection Accepted."]
    [(eqv? CONNACK_REFUSED_PROTOCOL_VERSION      connack_code)
     "Connection Refused: unacceptable protocol version."]
    [(eqv? CONNACK_REFUSED_IDENTIFIER_REJECTED   connack_code)
     "Connection Refused: identifier rejected."]
    [(eqv? CONNACK_REFUSED_SERVER_UNAVAILABLE    connack_code)
     "Connection Refused: broker unavailable."]
    [(eqv? CONNACK_REFUSED_BAD_USERNAME_PASSWORD connack_code)
     "Connection Refused: bad user name or password."]
    [(eqv? CONNACK_REFUSED_NOT_AUTHORIZED        connack_code)
     "Connection Refused: not authorised."]
    [else "Connection Refused: unknown reason."]))

(struct connection (uri port in out))

;; 连接服务端
(define (connect #:id [id "racket"]
                 #:version [version 'v3.1.1]
                 #:username [username false]
                 #:password [password false]
                 #:will-retain [will-retain false]
                 #:will-qos [will-qos false]
                 #:will-flag [will-flag false]
                 #:will-payload [will-payload false]
                 #:clean-session [clean-session true]
                 #:keepalive [keepalive 60])
  "")

(define (disconnect conn)
  conn)

(define (publish channel msg)
  "")

(define (subscribe channel)
  channel)

(define (unsubscribe channel)
  channel)

(define (connack conn
                 #:session-present [sp false])
  ;; 服务端发送CONNACK报文响应从客户端收到的CONNECT报文。服务端发送给客户端的第一个报文必须是CONNACK [MQTT-3.2.0-1]。
  ;; 如果客户端在合理的时间内没有收到服务端的CONNACK报文，客户端应该关闭网络连接。合理 的时间取决于应用的类型和通信基础设施。
  "")

;; 心跳请求
(define pingreq "")

;; 心跳响应
(define pingresp "")

(define (subscription-payload channel)
  channel)

(module+ test
  (require rackunit))
