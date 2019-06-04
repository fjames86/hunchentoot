;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :hunchentoot)

#+(or win32 windows)
(defun get-peer-address-and-port (socket)
  "Returns the peer address and port of the socket SOCKET as two
values.  The address is returned as a string in dotted IP address
notation, resp. IPv6 notation."
  (let ((addr (fsocket:socket-peer socket)))
    (values (let ((a (fsocket:sockaddr-in-addr addr)))
	      (format nil "~A.~A.~A.~A"
		      (aref a 0) (aref a 1) (aref a 2) (aref a 3)))
	    (fsocket:sockaddr-in-port addr))))

#+(or win32 windows)
(defun get-local-address-and-port (socket)
  "Returns the local address and port of the socket SOCKET as two
values.  The address is returned as a string in dotted IP address
notation, resp. IPv6 notation."
  (let ((addr (fsocket:socket-name socket)))
    (values (let ((a (fsocket:sockaddr-in-addr addr)))
	      (format nil "~A.~A.~A.~A"
		      (aref a 0) (aref a 1) (aref a 2) (aref a 3)))
	    (fsocket:sockaddr-in-port addr))))

;; Dirty hack incoming: we are given sockets set to ONONBLOCK but schannel doesn't support that...
;; Hence we spin while error status=EWOULDBLOCK until we have actually read some data. 
#+(or win32 windows)
(progn
  (defclass ononblock-tcp-stream (fsocket:tcp-stream)
    ())
  (defmethod trivial-gray-streams:stream-read-sequence ((stream ononblock-tcp-stream) seq start end &key)
    "Returns the index of last byte read."
    (declare (fixnum start end))
    (do ((done nil))
	(done done)
      (catch 'ewouldblock 
	(handler-bind 
	    (#+(or win32 windows)
	       (fsocket::win-error 
		(lambda (e)
		  (cond
		    ((= (fsocket::win-error-code e) fsocket::+wsa-error-wouldblock+)
		     (throw 'ewouldblock nil))))))
	  (let ((n (fsocket:socket-recv (fsocket::tcp-stream-fd stream) seq :start start :end end)))
	    (cond
	      ((> n 0)
	       (setf done (+ start n)))
	      (t 
	       (setf done start))))))))
  (defmethod trivial-gray-streams:stream-read-byte ((stream ononblock-tcp-stream))
    (do ((seq (make-array 1 :element-type '(unsigned-byte 8)))
	 (done nil))
	(done done)
      (catch 'ewouldblock 
	(handler-bind 
	    (#+(or win32 windows)
	       (fsocket::win-error 
		(lambda (e)
		  (cond
		    ((= (fsocket::win-error-code e) fsocket::+wsa-error-wouldblock+)
		     (throw 'ewouldblock nil))))))
	  (let ((n (fsocket:socket-recv (fsocket::tcp-stream-fd stream) seq)))
	    (cond
	      ((> n 0)
	       (setf done (aref seq 0)))
	      (t
	       (setf done :eof)))))))))



#+(or win32 windows)
(defun make-socket-stream (socket acceptor)
  "Returns a stream for the socket SOCKET.  The ACCEPTOR argument is
ignored."
  (declare (ignore acceptor))
  (make-instance 'ononblock-tcp-stream :fd socket))

#-(or win32 windows)
(defun get-peer-address-and-port (socket)
  "Returns the peer address and port of the socket SOCKET as two
values.  The address is returned as a string in dotted IP address
notation, resp. IPv6 notation."
  (multiple-value-bind (address port) (usocket:get-peer-name socket)
    (values (usocket::host-to-hostname address)
            port)))

#-(or win32 windows)
(defun get-local-address-and-port (socket)
  "Returns the local address and port of the socket SOCKET as two
values.  The address is returned as a string in dotted IP address
notation, resp. IPv6 notation."
  (multiple-value-bind (address port) (usocket:get-local-name socket)
    (values (usocket::host-to-hostname address)
            port)))

#-(or win32 windows)
(defun make-socket-stream (socket acceptor)
  "Returns a stream for the socket SOCKET.  The ACCEPTOR argument is
ignored."
  (declare (ignore acceptor))
  (usocket:socket-stream socket))

(defun make-lock (name)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  (bt:make-lock name))

(defmacro with-lock-held ((lock) &body body)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  `(bt:with-lock-held (,lock) ,@body))

(defun make-condition-variable (&key name)
  (declare (ignore name))
  (bt:make-condition-variable))

(defun condition-variable-signal (condition-variable)
  (bt:condition-notify condition-variable))

(defun condition-variable-wait (condition-variable lock)
  (bt:condition-wait condition-variable lock))
