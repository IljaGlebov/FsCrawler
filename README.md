# Crawler

Запуск 
```Crawler.exe Host=http://d3.ru SingleHost=true Depth=2 ResultPath=Dirty```


ссылки собираются из html, без учета ссылок которые могут быть получены ajax или сформированы javascript'ом

Тип ресурса определяется по MIME в ответе
```application/javascript
image/gif
image/jpeg
image/pjpeg
image/png 
image/tiff
text/css
text/html 
text/plain```

Загрузка в 20 потоков,