.PHONY: build
build:
	docker build -t mitchellh/workstation:dev .

.PHONY: push
push:
	docker tag mitchellh/workstation:dev mitchellh/workstation:latest
	docker push mitchellh/workstation:latest
