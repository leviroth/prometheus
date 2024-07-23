(** Collect metrics for Prometheus.
    See: https://prometheus.io/

    Notes:

    - The Prometheus docs require that client libraries are thread-safe. We interpret this to mean safe
      with Lwt threads, NOT with native threading.

    - This library is intended to be a dependency of any library that might need to report metrics,
      even though many applications will not enable it. Therefore it should have minimal dependencies.
*)

type metric_type =
  | Counter

module type NAME = sig
  type t = private string

  val v : string -> t
  (** Raises an exception if the name is not valid. *)

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int
end
(** A string that meets some additional requirements. *)

module MetricName : NAME
(** A valid name for a metric. *)

module LabelName  : NAME
(** A valid name for a label. *)

module MetricInfo : sig
  type t = {
    name : MetricName.t;
    metric_type : metric_type;
    help : string;
    label_names : LabelName.t list;
  }
end
(** Metadata about a metric. *)

module LabelSetMap : Asetmap.Map.S with type key = string list
(** A map indexed by a set of labels. *)

module MetricFamilyMap : Asetmap.Map.S with type key = MetricInfo.t
(** A map indexed by metric families. *)

module Sample_set : sig
  type sample = {
    ext : string;               (** An extension to append to the base metric name. *)
    value : float;
    bucket : (LabelName.t * float) option;   (** The "le" or "quantile" label and value, if any. *)
  }

  type t = sample list
  (** A collection of values that together represent a single sample.
      For a counter, each reading is just a single value, but more complex types
      require multiple values.
      For example, a "summary" sample set contains "_sum" and "_count" values.
   *)

  val sample : ?ext:string -> ?bucket:(LabelName.t * float) -> float -> sample
end

module CollectorRegistry : sig
  type t
  (** A collection of metrics to be monitored. *)

  type snapshot = Sample_set.t LabelSetMap.t MetricFamilyMap.t
  (** The result of reading a set of metrics. *)

  val create : unit -> t
  (** [create ()] is a fresh registry. This is mostly useful for testing. *)

  val default : t
  (** The default registry. *)

  val collect : t -> snapshot
  (** Read the current value of each metric. *)

  val register : t -> MetricInfo.t -> (unit -> Sample_set.t LabelSetMap.t) -> unit
  (** [register t metric collector] adds [metric] to the set of metrics being collected.
      It will call [collector ()] to collect the values each time [collect] is called. *)

  val register_pre_collect : t -> (unit -> unit) -> unit
  (** [register_pre_collect t fn] arranges for [fn ()] to be called at the start
      of each collection. This is useful if one expensive call provides
      information about multiple metrics. *)
end
(** A collection of metric reporters. Usually, only {!CollectorRegistry.default} is used. *)

module type METRIC = sig
  type family
  (** A collection of metrics that are the same except for their labels.
      e.g. "Number of HTTP responses" *)

  type t
  (** A particular metric.
      e.g. "Number of HTTP responses with code=404" *)

  val v_labels : label_names:string list -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> family
  (** [v_labels ~label_names ~help ~namespace ~subsystem name] is a family of metrics with full name
      [namespace_subsystem_name] and documentation string [help]. Each metric in the family will provide
      a value for each of the labels.
      The new family is registered with [registry] (default: {!CollectorRegistry.default}). *)

  val labels : family -> string list -> t
  (** [labels family label_values] is the metric in [family] with these values for the labels.
      The order of the values must be the same as the order of the [label_names] passed to [v_labels];
      you may wish to write a wrapper function with labelled arguments to avoid mistakes.
      If this is called multiple times with the same set of values, the existing metric will be returned. *)

  val v_label : label_name:string -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> (string -> t)
  (** [v_label] is a convenience wrapper around [v_labels] for the case where there is a single label.
      The result is a function from the single label's value to the metric. *)

  val v : ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> t
  (** [v] is a convenience wrapper around [v_labels] for the case where there are no labels. *)
end
(** Operations common to all types of metric. *)

module Counter : sig
  include METRIC
  val inc_one : t -> unit
  val inc : t -> float -> unit
  (** [inc t v] increases [t] by [v], which must be non-negative. *)
end
(** A counter is a cumulative metric that represents a single numerical value that only ever goes up. *)


