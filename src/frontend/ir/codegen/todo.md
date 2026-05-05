```js
var a = do {
	var b = 2;
	var c = 3;
	b + c;
};
```

```
push 2i
storelocal 2
push 3i
storelocal 1
loadlocal 1
loadlocal 2
add
storelocal 0
```

```js
var a = if true  {
	var b = 2;
	var c = 3;
	b + c;
} else  {
    50
};
```

```
push true
branch else@0
if@0:
push 2i
storelocal 2
push 3i
storelocal 1
loadlocal 1
loadlocal 2
add
else@0:
    push 50
endif@0:
storelocal 0
```

key takeaways, dont add pop at the end when some
