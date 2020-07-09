.PHONY: etcd
etcd:
	etcd \
             -name etcd0 \
             -advertise-client-urls http://127.0.0.1:2379,http://127.0.0.1:4001 \
             -listen-client-urls http://0.0.0.0:2379,http://0.0.0.0:4001 \
             -initial-advertise-peer-urls http://127.0.0.1:2380 \
             -listen-peer-urls http://0.0.0.0:2380 \
             -initial-cluster-token etcd-cluster-1 \
             -initial-cluster etcd0=http://127.0.0.1:2380 \
             -initial-cluster-state new --debug=true

PHONY: build
build:
	racket -e '(require "./main.rkt")'
	go fmt ./main.go
	go build -o main ./main.go

.PHONY: fmt
fmt:
	go fmt ./...

.PHONY: run
run: build fmt
	go run ./main.go
