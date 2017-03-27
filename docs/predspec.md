A cursor should support two operations:
advance forward and 

        export interface CursorLike<T> {
            next(): Maybe<CursorLike>;
            value(): T;
        }


- Cursor errors should contain a message.
- Cursor errors should

```
        (type template [^CursorT]
            
            
            ^CursorError (concept 
                :allocator  ^alloc/Allocator
                :cursor     ^CursorT
                :message    ^string
            ))
                
        export interface CursorError<Cursor> {
            // The cursor where this error occured
            cursor: Cursor,

            // Some custom message
            message: string,
        }
```