# Lock server and client

## Precaution

The lock server is not secure.
Use firewall to limit access to it.
Do not put it outside your private network.

## Common use cases

### Installing the server

Use [deploy-maven-plugin](/maven-plugin/deploy-maven-plugin) to install the lock server.

```
mkdir lock-server
cd lock-server
mvn \
    com.spacetimecat.maven.plugin:deploy-maven-plugin:0.0.0:deploy \
    -DgroupId=com.spacetimecat \
    -DartifactId=lock-server \
    -Dversion=0.0.0 \
    -DmainClass=com.spacetimecat.concurrent.lock.service.store.server.ServerMain
```

### Starting the server

Replace `Port` with the port you want the server to listen at.

```
cd lock-server
port=Port ./run
```

### The client

See [ClientMain.java](lock-example/src/main/java/com/spacetimecat/concurrent/lock/example/ClientMain.java).
