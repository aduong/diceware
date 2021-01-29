FROM ubuntu

RUN apt-get update && apt-get upgrade -y \
  && apt-get install -y curl gnupg2 tzdata \
  && echo "deb https://dl.bintray.com/sbt/debian /" >> /etc/apt/sources.list.d/sbt.list \
  && curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add \
  && apt-get update \
  && apt-get install -y \
    openjdk-14-jre \
    sbt \
  && :

WORKDIR /opt/code
ADD diceware.wordlist generate.scala ./

RUN sbt package

ENTRYPOINT [ \
 "java", \
 "-cp", \
 "/root/.sbt/boot/scala-2.12.12/lib/*:target/scala-2.12/classes", \
 "Diceware" \
]