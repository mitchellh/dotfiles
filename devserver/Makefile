.PHONY: build
build:
	docker build -t mitchellh/workstation:dev .

.PHONY: run
run:
	docker run -it --net=host --rm -v /var/run/docker.sock:/var/run/docker.sock --cap-add=SYS_PTRACE --security-opt seccomp=unconfined --privileged mitchellh/workstation:dev

.PHONY: push
push:
	docker tag mitchellh/workstation:dev mitchellh/workstation:latest
	docker push mitchellh/workstation:latest
