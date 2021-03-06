module type FUNCTOR = sig
  type +'a t
end

type (+'a, 's) io

type 's scheduler = {
  bind : 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io;
  return : 'a. 'a -> ('a, 's) io;
}

module type SCHEDULER = sig
  type +'a s
  type t

  external inj : 'a s -> ('a, t) io = "%identity"
  external prj : ('a, t) io -> 'a s = "%identity"
end

module type MUTEX = sig
  type +'a fiber
  type t

  val create : unit -> t
  val lock : t -> unit fiber
  val unlock : t -> unit
end

module type CONDITION = sig
  type +'a fiber
  type mutex
  type t

  val create : unit -> t
  val wait : t -> mutex -> unit fiber
  val signal : t -> unit
  val broadcast : t -> unit
end

module type IO = sig
  type +'a t

  module Mutex : MUTEX with type 'a fiber = 'a t

  module Condition :
    CONDITION with type 'a fiber = 'a t and type mutex = Mutex.t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val detach : (unit -> 'a) -> 'a t
  val parallel_map : f:('a -> 'b t) -> 'a list -> 'b list t
  val parallel_iter : f:('a -> unit t) -> 'a list -> unit t
end

module Make (T : FUNCTOR) : SCHEDULER with type 'a s = 'a T.t = struct
  type 'a s = 'a T.t
  type t

  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

module type UID = sig
  type t
  type ctx

  val empty : ctx
  val get : ctx -> t
  val feed : ctx -> ?off:int -> ?len:int -> Bigstringaf.t -> ctx
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val length : int
  val of_raw_string : string -> t
  val to_raw_string : t -> string
  val pp : t Fmt.t
  val null : t
end

type kind = [ `A | `B | `C | `D ]

let _max_depth = 60
