[System.Net.WebRequest]::DefaultWebProxy = New-Object System.Net.WebProxy("http://proxy-rie.http.insee.fr:8080")
$env:http_proxy = "http://proxy-rie.http.insee.fr:8080"
$env:https_proxy = "http://proxy-rie.http.insee.fr:8080"
$env:no_proxy = ""
