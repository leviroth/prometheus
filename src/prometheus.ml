open! Astring
open! Asetmap

module type NAME_SPEC = sig
  val valid : Re.re
end

module type NAME = sig
  type t = private string
  val v : string -> t
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
end

module Name(N : NAME_SPEC) : NAME = struct
  type t = string

  let v name =
    if not (Re.execp N.valid name) then
      failwith (Format.asprintf "Invalid name %S" name);
    name

  let compare = String.compare

  let pp = Format.pp_print_string
end

let alphabet = Re.(alt [ rg 'a' 'z'; rg 'A' 'Z' ])
module LabelName = struct
  (* "^[a-zA-Z_][a-zA-Z0-9_]*$" *)
  let start = Re.alt [ alphabet; Re.char '_' ]
  let rest  = Re.alt [ start; Re.digit ]
  include Name(struct let valid = Re.compile @@ Re.seq [ Re.bos; start; Re.rep rest; Re.eos] end)
end
module MetricName = struct
  (* "^[a-zA-Z_:][a-zA-Z0-9_:]*$"  *)
  let start = Re.alt [ LabelName.start; Re.char ':' ]
  let rest = Re.alt [ start; Re.digit ]
  include Name(struct let valid = Re.compile @@ Re.seq [ Re.bos; start; Re.rep rest; Re.eos] end)
end

type metric_type =
  | Counter

module LabelSet = struct
  type t = string list
  let compare (a:t) (b:t) = compare a b
end
module LabelSetMap = Map.Make(LabelSet)

module MetricInfo = struct
  type t = {
    name : MetricName.t;
    metric_type : metric_type;
    help : string;
    label_names : LabelName.t list;
  }

  let pp_opt () = function
    | None -> ""
    | Some v -> v ^ "_"

  let v ~help ?(label_names=[]) ~metric_type ?namespace ?subsystem name =
    let name = Printf.sprintf "%a%a%s" pp_opt namespace pp_opt subsystem name in
    {
      name = MetricName.v name;
      metric_type;
      help;
      label_names;
    }

  let compare a b = MetricName.compare a.name b.name
end

module MetricFamilyMap = Map.Make(MetricInfo)

module Sample_set = struct
  type sample = {
    ext : string;
    value : float;
    bucket : (LabelName.t * float) option;
  }

  type t = sample list

  let sample ?(ext="") ?bucket value = { ext; value; bucket }
end

module CollectorRegistry = struct
  type t = {
    mutable metrics     : (unit -> Sample_set.t LabelSetMap.t      ) MetricFamilyMap.t;
    mutable pre_collect     : (unit -> unit      ) list;
  }

  type snapshot = Sample_set.t LabelSetMap.t MetricFamilyMap.t

  let create () = {
    metrics = MetricFamilyMap.empty;
    pre_collect = [];
  }

  let default = create ()

  let register_pre_collect t f = t.pre_collect <- f :: t.pre_collect


  let ensure_not_registered t info =
    if MetricFamilyMap.mem info t.metrics
    then failwith (Format.asprintf "%a already registered" MetricName.pp info.MetricInfo.name)

  let register t info collector =
    ensure_not_registered t info;
    t.metrics <- MetricFamilyMap.add info collector t.metrics

  let collect t =
    List.iter (fun f -> f ()) t.pre_collect;
    MetricFamilyMap.map (fun f -> f ()) t.metrics 

end

module type METRIC = sig
  type family
  type t
  val v_labels : label_names:string list -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> family
  val labels : family -> string list -> t
  val v_label : label_name:string -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> (string -> t)
  val v : ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> t
end

module type CHILD = sig
  type t
  val create : unit -> t
  val values : t -> Sample_set.t
  val metric_type : metric_type
  val validate_label : string -> unit
end

module Metric(Child : CHILD) : sig
  include METRIC with type t = Child.t
end = struct
  type family = {
    metric : MetricInfo.t;
    mutable children : Child.t LabelSetMap.t;
  }

  type t = Child.t

  let collect t =
    LabelSetMap.map Child.values t.children

  let v_labels ~label_names ?(registry=CollectorRegistry.default) ~help ?namespace ?subsystem name =
    List.iter Child.validate_label label_names;
    let label_names = List.map LabelName.v label_names in
    let metric = MetricInfo.v ~metric_type:Child.metric_type ~help ~label_names ?namespace ?subsystem name in
    let t = {
      metric;
      children = LabelSetMap.empty;
    } in
    CollectorRegistry.register registry metric (fun () -> collect t);
    t

  let labels t label_values =
    assert (List.length t.metric.MetricInfo.label_names = List.length label_values);
    match LabelSetMap.find label_values t.children with
    | Some child -> child
    | None ->
      let child = Child.create () in
      t.children <- LabelSetMap.add label_values child t.children;
      child

  let v_label ~label_name ?registry ~help ?namespace ?subsystem name =
    let family = v_labels ~label_names:[label_name] ?registry ~help ?namespace ?subsystem name in
    fun x -> labels family [x]

  let v ?registry ~help ?namespace ?subsystem name =
    let family = v_labels ~help ?registry ?namespace ?subsystem name ~label_names:[] in
    labels family []
end

module Counter = struct
  include Metric(struct
      type t = float ref
      let create () = ref 0.0
      let values t = [Sample_set.sample !t]
      let metric_type = Counter
      let validate_label _ = ()
    end)

  let inc_one t =
    t := !t +. 1.0

  let inc t v =
    assert (v >= 0.0);
    t := !t +. v
end
