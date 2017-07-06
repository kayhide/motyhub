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

#### :bulb:Tips

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

## Admin frontend

データベースの内容を直接参照するために、 [PostgREST](https://github.com/begriffs/postgrest) を使う。

```
$ postgrest --help
Usage: postgrest FILENAME
  PostgREST 0.4.2.0 / create a REST API to an existing Postgres database
...

$ postgrest db/postgrest-development.conf
Listening on port 8081
Attempting to connect to the database...
Connection successful
```

`motyhub-admin` に移動する。

```
$ cd motyhub-admin
```

node パッケージのインストールと開発サーバーの起動をする。

```
$ yarn
$ yarn start
```

#### :bulb:Tips

Mac であれば PostgREST は `brew install postgrest` でインストールできる。
