default:
	@echo options: prereqs run

prereqs:
	pip install -r pythonPrerequisites.txt

run:
	(export FLASK_APP=serveStaticGenomeFiles.py; flask run -p 60050 --host=0.0.0.0)
