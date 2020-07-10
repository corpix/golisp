.DEFAULT_GOAL := all

## parameters

NAME              ?= revip
PROJECT           ?= corpix
VERSION           ?= development
ENV               ?= dev

PARALLEL_JOBS ?= 8
NIX_OPTS      ?=

export GOFLAGS ?=

## bindings

root                := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
nix_dir             := nix
pkg_prefix          := github.com/$(PROJECT)/$(NAME)
tmux                := tmux -2 -f $(PWD)/.tmux.conf -S $(PWD)/.tmux
tmux_session        := $(PROJECT)/$(NAME)
nix                 := nix $(NIX_OPTS)
shell_volume_nix    := nix
shell_volume_go     := pkg

### reusable and long opts for commands inside rules

add_shell_opts ?=
shell_opts = -v $(shell_volume_nix):/nix:rw \
	-v $(shell_volume_go):/root/go/pkg:rw   \
	-v $(root):/chroot                      \
	-e COLUMNS=$(COLUMNS)                   \
	-e LINES=$(LINES)                       \
	-e TERM=$(TERM)                         \
	-e NIX_BUILD_CORES=$(NIX_BUILD_CORES)   \
	-w /chroot                              \
	--hostname $(PROJECT).localhost         \
	$(foreach v,$(ports), -p $(v):$(v) ) $(add_shell_opts)

lint_opts = --color=always                                                     \
	--exclude='uses unkeyed fields'                                            \
	--exclude='type .* is unused'                                              \
	--exclude='should merge variable declaration with assignment on next line' \
	--deadline=120s                                                            \

## helpers

, = ,

## macro

# XXX: yes, this two empty lines here are required :)
define \n


endef

define sorted
@echo $(cmds) | sed 's|\s|\n|g' | sort
endef

define fail
{ echo "error: "$(1) 1>&2; exit 1; }
endef

define expect
{ grep $(1) > /dev/null || $(call fail,$(2)); }
endef

define required
@if [ -z $(2) ]; then $(call fail,"$(1) is required") fi
endef

define test_run
bash --noprofile -euxo pipefail -c "$(3) go test $(2) $(1) | grep -vF '[no test files]'"
endef

define lint_run
golangci-lint $(lint_opts) run $(1)
endef

## targets

.PHONY: all
all: test lint build # test, check and build all cmds

.PHONY: help
help: # print defined targets and their comments
	@grep -Po '^[a-zA-Z%_/\-\s]+:+(\s.*$$|$$)' $(MAKEFILE_LIST)  \
		| sort                                                   \
		| sed 's|:.*#|#|;s|#\s*|#|'                              \
		| column -t -s '#' -o ' | '

### releases

### development

#### testing

.PHONY: test
test: # test whole repository
	$(call test_run,./...,)

.PHONY: lint
lint: # lint whole repository
	$(call lint_run,./...)

#### environment management

.PHONY: dev/clean
dev/clean: # clean development environment artifacts
	docker volume rm nix
	docker volume rm pkg

.PHONY: dev/shell
dev/shell: $(contracts_submodule)/.git # run development environment shell
	@docker run --rm -it               \
		--log-driver=none              \
		$(shell_opts) nixos/nix:latest \
		nix-shell --command "exec make dev/start-session"

.PHONY: dev/shell/raw
dev/shell/raw: $(contracts_submodule)/.git # run development environment shell
	@docker run --rm -it               \
		--log-driver=none              \
		$(shell_opts) nixos/nix:latest \
		nix-shell

.PHONY: dev/session
dev/start-session: # start development environment terminals with database, blockchain, etc... one window per app
	@$(tmux) has-session    -t $(tmux_session) && $(call fail,tmux session $(tmux_session) already exists$(,) use: '$(tmux) attach-session -t $(tmux_session)' to attach) || true
	@$(tmux) new-session    -s $(tmux_session) -n console -d
	@sleep 1 # sometimes input is messed up (bash+stdin early handling?)
	@$(tmux) send-keys      -t $(tmux_session):0 C-z 'emacs .' Enter
	@$(tmux) select-window  -t $(tmux_session):0

	@if [ -f $(root)/.personal.tmux.conf ]; then         \
		$(tmux) source-file $(root)/.personal.tmux.conf; \
	fi

	@$(tmux) attach-session -t $(tmux_session)

.PHONY: dev/attach-session
dev/attach-session: # attach to development session if running
	@$(tmux) attach-session -t $(tmux_session)

.PHONY: dev/stop-session
dev/stop-session: # stop development environment terminals
	@$(tmux) kill-session -t $(tmux_session)

.PHONY: fmt
fmt: # go fmt repository
	go fmt ./...

.PHONY: clean
clean: # clean stored state
	rm -rf result*

