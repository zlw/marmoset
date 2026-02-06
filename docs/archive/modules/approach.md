# Module System

Context: so far, Marmoset has a simple single-file mode. We can't load other files, nor packages.
In the future, we want to be able to import other Marmoset files. Since our backend is Go, we should be able to call Go packages as well.

## Syntax

We're following Scala import syntax. We want folder structure to map to module strucure implicitly, no need to specify package name like Java/Kotlin/Scala
We do not allow wildcard imports, as we want to keep things explicit. We do not allow circular imports.

```marmoset
import iam.rbac.infrastructure.repositories.users               // available as users.get_by_id
import iam.rbac.infrastructure.repositories.users.{get_by_id}  // available as get_by_id
```

## Calling Go Packages

We can call Go packages by importing them with their module path.

```marmoset
import go.google.uuid // available as uuid.new
```

now, this would need to map to google.com/google/uuid package in Go, but not everyone is using Github so it cannot be hardcoded.
We might need a `Gemfile` file or smth? Wait, there's go.mod or smth in Golang where we specify packages and versions, so we could have something similar in marmoset:

```marmoset
go google.uuid = github.com/google/uuid v1.3.0
mr some.lib = github.com/some/lib v0.1.0
```

or something alsong the lines. This way we can have a clear mapping from marmoset imports to Go packages, and we can also manage versions easily.

now, we also have a problem of type signatures and things like that.
