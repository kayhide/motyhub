# motyhub

## Database setup

Postgresql を使う。事前にサーバーを起動しておく。

```
$ psql -c "SELECT 1;"
 ?column?
----------
        1
(1 row)
```

データベーススキーマは [Standalone Migrations](https://github.com/thuss/standalone-migrations) で管理する。

`gem` のインストール、ユーザーの作成、データベースの作成、スキーマのロードをする。

```
$ bundle
$ bundle exec rake db:create_user
$ bundle exec rake db:create
$ bundle exec rake db:schema:load
```

### Tips

`bin` に `bundle exec` をラップした `rake` コマンドがある。
`./bin` を `PATH` の前の方に加えておけば、 `bundle exec` を省いて `rake` できる。

```
$ PATH=./bin:$PATH
$ which rake
./bin/rake
```

## Server setup

`motyhub-app` に移動する。

```
$ cd motyhub-app
```

ビルドする。

```
$ stack build
```

起動する。

```
$ stack exec motyhub
running motyhub on port 8080...
```

## Development

`motyhub-app` に移動する。

```
$ cd motyhub-app
```

### Development server

`ghcid` を使ってオートリロードする。

```
$ ghcid --test="DevMain.reload"
```

### Repl

Repl では `src/Dev.hs` をロードする。
Repl でよく使うモジュールは `src/Dev.hs` でインポートする。

```
$ stack ghci src/Dev.hs
```
