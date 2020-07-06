UTILSLIB = ${HOME}/Lib/Scala/Utils/jmutils.jar
SCALAC = scalac -feature -cp ${UTILSLIB}
WEBSTATIC=jmaraist@docker.cs.uwlax.edu:/home/jmaraist/docker/wordpress-maraist/srv/html/u/advisobot
EXPORT=advisobot-doc.zip advisobot.jar \
	 advisobot-sample-src.tgz advisobot-sample.pdf
SOURCEFILES=src/main/scala/advisobot/*.scala src/main/scala/advisobot/uwlax/*.scala
SOURCEPATH=src

build: advisobot.jar
	#

advisobot.jar: ${SOURCEFILES}
	${SCALAC} -d $@ $^ -sourcepath src

doc: .doc-build
.doc-build: ${SOURCEFILES} src/root.scaladoc
	@mkdir -p doc
	scaladoc -diagrams -diagrams-dot-path /usr/bin/dot \
		-doc-external-doc:${UTILSLIB}#file:///home/jm/Lib/Scala/utils/doc,/usr/share/scala-2.11/lib/scala-library.jar#file:///usr/share/doc/scala-2.11/api/library \
		-doc-title "The Advise-O-Bot" \
		-doc-root-content src/root.scaladoc -d doc \
		-classpath ${UTILSLIB} \
		-sourcepath ${SOURCEPATH} \
		${SOURCEFILES}
	@touch $@

advisobot-sample-src.tgz:
	rm -rf sample/reports/* sample/*~
	tar czf advisobot-sample-src.tgz sample

advisobot-sample.pdf:
	cd sample; rm -f .reports; make
	pdfjam -q -o $@ sample/reports/*.pdf

clean:
	rm -f ${EXPORT} .doc-build

advisobot-doc.zip: .doc-build
	zip $@ -r doc

export: ${EXPORT}
	scp $^ ${WEBSTATIC}

