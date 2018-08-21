(* translated from http://bugsquash.blogspot.com/2011/11/lenses-in-f.html *)

type ('a, 'b) lens = {
  get: 'a -> 'b;
  set: 'b -> 'a -> 'a;
}

let update l f a =
  let value = l.get a in
  let value' = f value in
  l.set value' a

let (>>|) l1 l2 = {
  get = (fun x -> l2.get (l1.get x));
  set = (fun x -> update l1 (l2.set x));
}


type car = {
  make: string;
  model: string;
  mileage: int;
}

let make = {
  get = (fun {make; _} -> make);
  set = fun make c -> {c with make}
}

let model = {
  get = (fun {model; _} -> model);
  set = fun model c -> {c with model}
}

let mileage = {
  get = (fun {mileage; _} -> mileage);
  set = fun mileage c -> {c with mileage}
}


type editor = {
  name: string;
  salary: int;
  car: car;
}

let name = {
  get = (fun {name; _} -> name);
  set = fun name e -> {e with name}
}

let salary = {
  get = (fun {salary; _} -> salary);
  set = fun salary e -> {e with salary}
}

let car = {
  get = (fun {car; _} -> car);
  set = fun car e -> {e with car}
}


type book = {
  name: string;
  author: string;
  editor: editor;
}

let name = {
  get = (fun {name; _} -> name);
  set = fun name b -> {b with name}
}

let author = {
  get = (fun {author; _} -> author);
  set = fun author b -> {b with author}
}

let editor = {
  get = (fun {editor; _} -> editor);
  set = fun editor b -> {b with editor}
}


let b = {
  name="The Lord of the Rings";
  author="J. R. R. Tolkien";
  editor={
    name="Allen & Unwin";
    salary=42;
    car={
      make="Ford";
      model="T";
      mileage=10000;
    }
  }
}

let b' = update (editor >>| car >>| make) String.uppercase_ascii b
