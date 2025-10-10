type loc

exception UnexpectedToken of { token : string; location : loc }
exception UnterminatedComment of { started_at : loc }

val token : Sedlexing.lexbuf -> Parser.token
