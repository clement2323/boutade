install.packages("devtools")
devtools::document("codes/BoutadE")
devtools::check("codes/BoutadE")
devtools::load_all("codes/BoutadE")
getwd()
setwd('codes/BoutadE')
usethis::use_package('openxlsx')

[System.Net.WebRequest]::DefaultWebProxy = New-Object System.Net.WebProxy("http://proxy-rie.http.insee.fr:8080")
$env:http_proxy = "http://proxy-rie.http.insee.fr:8080"
$env:https_proxy = "http://proxy-rie.http.insee.fr:8080"
$env:no_proxy = ""
