documentation(maven_coordinates/3, [
    summary-string("the program's Maven coordinates")
    , detail-string("
GroupId is reverse domain name such as \"com.example\".

ArtifactId is the project name when the pom.xml is opened in IntelliJ IDEA.

Version is a string like \"1.23.456\":
a string with three dot-separated decimal components.

Try to stick to that form, because Maven version comparison may be surprising otherwise.
")
]).
