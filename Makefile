PROJECT := wai-middleware-validation
BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
GHC_VER := 8.8
RESOLVER := lts-16.31
NAMESPACE := kyotsuya
BASE_IMAGE := $(NAMESPACE)/$(PROJECT)-test:$(RESOLVER)
export COMPOSE_PROJECT_NAME := $(PROJECT)-$(BRANCH)

.PHONY: env
env:
	@echo "PROJECT=$(PROJECT)"
	@echo "BRANCH=$(BRANCH)"
	@echo "GHC_VER=$(GHC_VER)"
	@echo "RESOLVER=$(RESOLVER)"
	@echo "NAMESPACE=$(NAMESPACE)"
	@echo "BASE_IMAGE=$(BASE_IMAGE)"
	@echo "COMPOSE_PROJECT_NAME=$(COMPOSE_PROJECT_NAME)"

.PHONY: base
base:
	docker build -t $(BASE_IMAGE) --progress plain --build-arg GHC_VER=$(GHC_VER) --build-arg RESOLVER=$(RESOLVER) ./docker/test-base

.PHONY: base-bash
base-bash:
	docker run -it --rm $(BASE_IMAGE) /bin/bash

.PHONY: base-push
base-push:
	docker push $(BASE_IMAGE)

.PHONY: build
build:
	docker-compose build --progress plain --build-arg BASE_IMAGE=$(BASE_IMAGE)

.PHONY: test
test: build
	docker-compose run --rm dev stack test

.PHONY: bash
bash:
	docker-compose run --rm dev /bin/bash

.PHONY: ps
ps:
	docker-compose ps

.PHONY: down
down:
	docker-compose down
