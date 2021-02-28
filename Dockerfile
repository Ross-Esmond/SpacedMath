FROM openjdk:8-alpine

COPY target/uberjar/spacedmath.jar /spacedmath/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/spacedmath/app.jar"]
