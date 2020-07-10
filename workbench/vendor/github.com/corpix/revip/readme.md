``` console
~/p/s/g/corpix/revip $ bash -c 'REVIP_BAZ=777 REVIP_FOO_BAR=qux REVIP_DOX=1,2,3 go test -v ./...'
=== RUN   TestRevip
-- revip.Config{Foo:revip.Foo{Bar:"qux", Qux:false}, Baz:777, Dox:[]string{"1", "2", "3"}, Box:[]int{666, 777, 888}}
--- PASS: TestRevip (0.00s)
PASS
ok  	github.com/corpix/revip	(cached)
```
