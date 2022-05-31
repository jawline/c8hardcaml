open! Core
open! Hardcaml
open! Signal
open Global

(** An implementation of a simple pseudorandom number generator
    (XOR-shift 64) in hardware for the random opcodes.
    
    If the seed input is not zero then the current state
    will be overwritten with the new seed, otherwise a new random
    number will be generated each cycle. *)

module I = struct
  type 'a t =
    { 
    clock : 'a [@bits 1]
    ; clear : 'a [@bits 1] 
    ; seed : 'a [@bits 64]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { pseudo_random : 'a [@bits 64] }
  [@@deriving sexp_of, hardcaml]
end

let create (i : _ I.t) =
  let open Always in
  let open Variable in
  let pseudo_random = reg ~enable:vdd ~width:64 r_sync in
  let next = pseudo_random.value in
  let next = next ^: (sll next 13) in
  let next = next ^: (srl next 7) in
  let next = next ^: (sll next 17) in
  let next_value = wire ~default:next in
  compile [
   when_ (i.seed <>:. 0) [ next_value <-- i.seed ]
   ; pseudo_random <-- next_value.value
  ];
  { O.pseudo_random = next_value.value }
;;

module Test = struct

        let sim_set_seed ~seed (i : _ I.t) =
            i.seed := Bits.of_int ~width:64 seed
        ;;


  let cycle sim (o : _ O.t) =
    Cyclesim.cycle sim;
    Core.print_s [%message "" ~pseudo_random:(Bits.to_int !(o.pseudo_random) : int)];;
  
  let test ~seed ~cycles =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in

    sim_set_seed ~seed inputs;

    (* Cycle twice to show that the random number generator does not start
       when seed is set *)
    cycle sim outputs;
    cycle sim outputs;
    (* Reset seed to zero and generate ~cycle random values *)
    sim_set_seed ~seed:0 inputs;
    Sequence.range 0 cycles |> Sequence.iter ~f:(fun _ ->
            cycle sim outputs;
    );
    () 
  ;;

  let%expect_test "seed and cycle" =
    test ~seed:967531223135 ~cycles:128; 
    [%expect {|
      (pseudo_random 967531223135)
      (pseudo_random 967531223135)
      (pseudo_random 2482561710366037723)
      (pseudo_random -2821728140033329866)
      (pseudo_random 3381513551776863580)
      (pseudo_random -2993722293645530378)
      (pseudo_random 4249267510368762147)
      (pseudo_random -1151875455053916399)
      (pseudo_random -3545341970826856065)
      (pseudo_random 4264953125363019597)
      (pseudo_random -2812446833441639213)
      (pseudo_random -4527595793815391238)
      (pseudo_random -3784639190650379235)
      (pseudo_random -340588717576092395)
      (pseudo_random -1202714723245203873)
      (pseudo_random 2191374455794140451)
      (pseudo_random -3632721436417696023)
      (pseudo_random 3569978878405248020)
      (pseudo_random 3725712344880807196)
      (pseudo_random 3180746587244554982)
      (pseudo_random -159614491943408205)
      (pseudo_random -2263717540960334936)
      (pseudo_random -2632263047041606721)
      (pseudo_random -3315540437908262584)
      (pseudo_random 2700180976052056746)
      (pseudo_random 2708387420503272271)
      (pseudo_random -2184267637560135847)
      (pseudo_random -1387021846703511481)
      (pseudo_random 3436143133380580951)
      (pseudo_random 91996190692375483)
      (pseudo_random -436954958897863284)
      (pseudo_random 154825853390479047)
      (pseudo_random 2557878309111941818)
      (pseudo_random -4103811135862328657)
      (pseudo_random -1875575867188167414)
      (pseudo_random 2232099186663741424)
      (pseudo_random 1643165328284001375)
      (pseudo_random 2824923286615418327)
      (pseudo_random 2120670707220976092)
      (pseudo_random 2883656148892815871)
      (pseudo_random 630836679073156892)
      (pseudo_random 4256277900351498882)
      (pseudo_random 2861333385219448279)
      (pseudo_random 1669481766675478828)
      (pseudo_random -4109076397667749922)
      (pseudo_random -3363941230320518015)
      (pseudo_random -1826726069998611400)
      (pseudo_random 1439379060657752336)
      (pseudo_random 4409191079489766658)
      (pseudo_random -763588586255574136)
      (pseudo_random 893231958697122495)
      (pseudo_random -2699835984485416118)
      (pseudo_random 3617465223657442764)
      (pseudo_random 346290660317094487)
      (pseudo_random -1720608754302292141)
      (pseudo_random 979461795386040141)
      (pseudo_random -2110131735079979469)
      (pseudo_random 2256260664458220575)
      (pseudo_random -2885689911194450953)
      (pseudo_random 1907059667528530224)
      (pseudo_random -763309644898733246)
      (pseudo_random -443293350205407452)
      (pseudo_random -1016778005971035302)
      (pseudo_random 3381739578416847084)
      (pseudo_random -2828687225955862691)
      (pseudo_random -226435313821602189)
      (pseudo_random -2674432652759347985)
      (pseudo_random -2900602434033732938)
      (pseudo_random 2308434082099154259)
      (pseudo_random -4014831870562656247)
      (pseudo_random 871478457496593929)
      (pseudo_random -2539741319286276475)
      (pseudo_random -3183573153214029384)
      (pseudo_random 1685120329027954171)
      (pseudo_random -1048719734293841040)
      (pseudo_random 3521221712313390342)
      (pseudo_random -2050864716945080660)
      (pseudo_random 999209255082075089)
      (pseudo_random -3782717605770489386)
      (pseudo_random 3752894591692929653)
      (pseudo_random 1920611807324006849)
      (pseudo_random -2587383019714357678)
      (pseudo_random 1689881666789022334)
      (pseudo_random -4268026274875700534)
      (pseudo_random 1952849625798015071)
      (pseudo_random -1116669982790325137)
      (pseudo_random -251817377539754377)
      (pseudo_random 2810598883228613211)
      (pseudo_random 3182790909610257007)
      (pseudo_random -2358377369166986373)
      (pseudo_random 3553765697642171197)
      (pseudo_random -522259820186650517)
      (pseudo_random 3786099542954828763)
      (pseudo_random -4181680826255090724)
      (pseudo_random -2075439167406494357)
      (pseudo_random -2541399394482516751)
      (pseudo_random 450249264439878824)
      (pseudo_random 1704611198880279905)
      (pseudo_random -4470126761894295557)
      (pseudo_random 1638165628413344948)
      (pseudo_random -4035142183777535923)
      (pseudo_random 1774337799332837117)
      (pseudo_random -1357933247258183216)
      (pseudo_random 4583574756538268323)
      (pseudo_random -4412458018444795882)
      (pseudo_random 1525860424993499606)
      (pseudo_random 2262982752128990341)
      (pseudo_random -2693977119319487940)
      (pseudo_random -603583469133998040)
      (pseudo_random 3014769856917054104)
      (pseudo_random 1223372149857400285)
      (pseudo_random -2429200323057199882)
      (pseudo_random -1461352686947173409)
      (pseudo_random 1443521897367309032)
      (pseudo_random 3627389116056475645)
      (pseudo_random 1729259268922481810)
      (pseudo_random 90688041809551915)
      (pseudo_random -4441715680710617425)
      (pseudo_random 2879223568852431170)
      (pseudo_random 3873468102610084512)
      (pseudo_random 393354918756747365)
      (pseudo_random 3053120458277059893)
      (pseudo_random 2175009154404137359)
      (pseudo_random -1700124390399521732)
      (pseudo_random -150113635806066780)
      (pseudo_random 4261231641478726187)
      (pseudo_random 830317808870659567)
      (pseudo_random -3892807215756944636)
      (pseudo_random -3507242894913966158)
      (pseudo_random 3646334684634481901) |}]
  ;;
end
