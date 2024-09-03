# Haskell to Rust: Logical Pattern Translation

When porting a Haskell application to Rust, several key differences in language features and paradigms need to be addressed.

## 1. Monads and Context Handling

### Haskell Approach
In Haskell, monads provide a way to structure computations, offering sequential execution and a hidden context.

```haskell
data Config = Config { dbUrl :: String, apiKey :: String }

runWithConfig :: Config -> IO ()
runWithConfig config = do
  result <- fetchData (dbUrl config)
  processResult (apiKey config) result
```

### Rust Translation
Rust, being imperative, handles sequential execution naturally. The "hidden context" often becomes an explicit parameter, part of a struct, or a global variable.

1. Explicit parameter:

```rust
fn run_with_config(config: &Config) -> Result<(), Error> {
    let result = fetch_data(&config.db_url)?;
    process_result(&config.api_key, result)
}
```

2. Method on a struct:

```rust
struct AppState {
    config: Config,
}

impl AppState {
    fn run(&self) -> Result<(), Error> {
        let result = fetch_data(&self.config.db_url)?;
        process_result(&self.config.api_key, result)
    }
}
```

3. Global variable (using `once_cell` for safe initialization):

```rust
use once_cell::sync::Lazy;

static CONFIG: Lazy<Config> = Lazy::new(|| {
    Config {
        db_url: std::env::var("DB_URL").expect("DB_URL not set"),
        api_key: std::env::var("API_KEY").expect("API_KEY not set"),
    }
});

fn run() -> Result<(), Error> {
    let result = fetch_data(&CONFIG.db_url)?;
    process_result(&CONFIG.api_key, result)
}
```

### Short-circuiting with `?` operator
Rust's `?` operator provides a concise way to propagate errors, similar to Haskell's monadic bind (`>>=`) for `Result` types:

```rust
fn complex_operation() -> Result<String, Error> {
    let data = fetch_data()?;
    let processed = process_data(data)?;
    Ok(format!("Result: {}", processed))
}
```

This is equivalent to the following Haskell code:

```haskell
complexOperation :: IO (Either Error String)
complexOperation = do
  data <- fetchData
  processed <- processData data
  return $ Right $ "Result: " ++ processed
```

## 2. Type-Level Values and Proc Macros

### Haskell Approach
Haskell allows values to be promoted to the type level, which is particularly useful in libraries like Servant for defining routes.

```haskell
type UserAPI = "users" :> Get '[JSON] [User]
           :<|> "user" :> Capture "userId" Int :> Get '[JSON] User
```

### Rust Translation
Rust doesn't support value-level promotion to types. Instead, proc macros are used to achieve similar functionality.

```rust
#[derive(Router)]
struct UserAPI {
    #[router(GET, "/users")]
    get_users: Handler<Vec<User>>,
    #[router(GET, "/user/{userId}")]
    get_user: Handler<User, Path<i32>>,
}
```

In this case, the proc macro `Router` would inspect the struct fields and their attributes to generate the necessary routing and deserialization logic. The macro might generate code similar to:

```rust
impl UserAPI {
    fn route(&self, request: &Request) -> Result<Response, Error> {
        match (request.method(), request.path()) {
            (Method::GET, "/users") => self.get_users.handle(request),
            (Method::GET, path) if path.starts_with("/user/") => {
                let user_id = path.trim_start_matches("/user/")
                    .parse::<i32>()
                    .map_err(|_| Error::InvalidUserId)?;
                self.get_user.handle(request, Path(user_id))
            },
            _ => Err(Error::NotFound),
        }
    }
}
```

This generated code would handle routing and parameter extraction based on the struct definition and attributes.

## 3. Control Flow Inversion and Function Composition

A fundamental difference between Haskell and Rust lies in their execution models and how they handle control flow, particularly in the context of function composition.

### Haskell: Function Composition and Lazy Evaluation

Haskell's functional nature and lazy evaluation allow for elegant function composition:

```haskell
processData :: [Int] -> Int
processData = sum . filter even . map (*2)

main :: IO ()
main = do
    let numbers = [1..10]
    print $ processData numbers
```

In this Haskell code, `processData` is a composition of functions. The actual computation is deferred until the result is needed, allowing for efficient processing of potentially infinite lists.

### Non-Iterator Function Composition Example

Let's consider a more complex example of function composition that doesn't rely on iterators:

```haskell
import Data.Char (toUpper)

-- Individual functions
removeNonAlpha :: String -> String
removeNonAlpha = filter isAlpha

capitalize :: String -> String
capitalize = map toUpper

reverseString :: String -> String
reverseString = reverse

addPrefix :: String -> String
addPrefix s = "Processed: " ++ s

-- Composed function
processString :: String -> String
processString = addPrefix . reverseString . capitalize . removeNonAlpha

main :: IO ()
main = do
    let input = "Hello, World! 123"
    putStrLn $ processString input
    -- Output: "Processed: DLROWOLLEH"
```

In this Haskell example, we compose several string processing functions using the `.` operator, creating a pipeline of operations that are applied from right to left.

### Rust: Explicit Function Calls

Rust doesn't have built-in function composition operators like Haskell. Instead, we typically use explicit function calls or method chaining. Here's how we might translate the above Haskell code to Rust:

```rust
// Individual functions
fn remove_non_alpha(s: &str) -> String {
    s.chars().filter(|c| c.is_alphabetic()).collect()
}

fn capitalize(s: &str) -> String {
    s.to_uppercase()
}

fn reverse_string(s: &str) -> String {
    s.chars().rev().collect()
}

fn add_prefix(s: &str) -> String {
    format!("Processed: {}", s)
}

// Composed function
fn process_string(s: &str) -> String {
    let step1 = remove_non_alpha(s);
    let step2 = capitalize(&step1);
    let step3 = reverse_string(&step2);
    add_prefix(&step3)
}

fn main() {
    let input = "Hello, World! 123";
    println!("{}", process_string(input));
    // Output: "Processed: DLROWOLLEH"
}
```

In this Rust version:

1. We define each function separately, similar to the Haskell version.
2. Instead of using the `.` operator for composition, we explicitly call each function and store intermediate results.
3. The `process_string` function applies the operations in a step-by-step manner, making the execution order explicit.

### Alternative Rust Approach: Method Chaining

We can make the Rust code more concise and closer to the Haskell version by using method chaining:

```rust
fn process_string(s: &str) -> String {
    s.chars()
        .filter(|c| c.is_alphabetic())
        .collect::<String>()
        .to_uppercase()
        .chars()
        .rev()
        .collect::<String>()
}

fn main() {
    let input = "Hello, World! 123";
    println!("Processed: {}", process_string(input));
    // Output: "Processed: DLROWOLLEH"
}
```

This version uses method chaining to achieve a more functional style. While it's more concise, it can be harder to debug or modify or understand compile time errors because of complex types.
