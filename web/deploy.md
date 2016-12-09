# Deploying

**Note**: Do not edit pom.xml files manually.

## Clean up (usually unnecessary)

If everything else fails,
you may have to clean up a bit:

```
rm -r ~/.m2/repository/com/spacetimecat
```

To delete all pom.xml files,
cd to the directory containing this readme,
and run this:

```
find -name pom.xml -delete
```

## Generate pom.xml files

Use this custom Gradle invocation.

```
gradle generatePom
```

## Give Maven the secrets

Create a settings.xml file to store your repository login information.
You can place this file anywhere.

```
<?xml version="1.0" encoding="UTF-8" ?>
<settings xmlns="http://maven.apache.org/SETTINGS/1.0.0">
    <servers>
        <server>
            <id>spacetimecat</id>
            <username>USERNAME</username>
            <password>PASSWORD</password>
        </server>
    </servers>
</settings>
```

## Upload everything

Run the following command
in the directory containing this readme.
Replace `SETTINGS` with the path to the above settings.xml file.
Replace `URL` with that of your repository.

```
mvn -s SETTINGS deploy -DaltDeploymentRepository=spacetimecat::default::URL -DuniqueVersion=false
```
