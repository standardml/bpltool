signature BG_ADT =
sig
  type info
  type ppstream

  (** Page width used by bdnfToString. *)
  val pageWidth : int ref
  (** Block indentation used by bdnfToString. *)
  val indent : int ref

  (** Bigraph terms (possibly not well-formed). *)
  type bgterm
  (** Bigraph values (well-formed, possibly not BDNF). *)
  type bgval
  (** BDNF classes B, D, P and M *)
  type B type D type P type M 
  (** Bigraphs on BBDNF, DBDNF, PBDNF or MBDNF. *)
  type 'class bgbdnf

  structure BgTerm : BGTERM
  structure BgVal  : BGVAL
  structure BgBDNF : BGBDNF

  structure Interface   : INTERFACE
  structure Ion         : ION
  structure Permutation : PERMUTATION
  structure Wiring      : WIRING

  structure Control : CONTROL
  structure NameSet : MONO_SET
  structure LinkSet : MONO_SET

  structure Name : NAME
  structure Link : LINK

  structure Sugar : SUGAR
  structure BGErrorHandler : BGERRORHANDLER

end
