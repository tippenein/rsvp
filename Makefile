.PHONY: check clean docs format image lint

BUILD_IMAGE = fpco/stack-build:lts-7.3
IMAGE_NAME := rsvp
IMAGE_TAG := $(shell ./scripts/image-tag)
EXE_NAME := rsvp

LOCAL_BINARY_PATH = $(shell stack path --local-install-root)
LINUX_BINARY_PATH = $(shell stack --docker path --local-install-root)

check:
	stack test --fast

clean:
	stack clean
	stack --docker clean

docs:
	stack haddock

lint:
	hlint -q .

image:
	stack --docker build
	./scripts/build-image \
		$(BUILD_IMAGE) \
		$(LINUX_BINARY_PATH)/bin/$(EXE_NAME) \
		$(IMAGE_NAME) \
		$(IMAGE_TAG)

$(LOCAL_BINARY_PATH)/bin/$(EXE_NAME):
	stack build

bash_completion.sh: $(LOCAL_BINARY_PATH)/bin/$(EXE_NAME)
	stack exec $(EXE_NAME) -- --bash-completion-script $(LOCAL_BINARY_PATH)/bin/$(EXE_NAME) > bash_completion.sh
