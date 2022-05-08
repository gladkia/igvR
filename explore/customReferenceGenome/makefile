PORT = 8989

default:
	@echo targets: startServer openDemo

startServer:
	python3 -m http.server $(PORT)

openDemo:
	open http://localhost:$(PORT)/customReferenceGenome.html
