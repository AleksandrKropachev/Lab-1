open System


let rec first result =
  printf "Введите число (0 — переход ко второму): "
  let number = int(Console.ReadLine())

  if number = 0 then
    result
  else
    let newResult =
      if number % 2 = 0 then
        result @ [1]
      else
        result @ [0]

    printfn "Текущий список: %A" newResult

    first newResult



let rec chet x result =
  if x = 0 then result
  else
    let digit = x % 10
    let rest = x / 10

    if digit % 2 = 0 then
      chet rest (digit :: result)
    else
      chet rest result

let prov n =
  if n = 0 then [0]
  else chet (abs n) []



type Complex = {
  Re: float
  Im: float
}

let add z1 z2 =
  { Re = z1.Re + z2.Re
    Im = z1.Im + z2.Im }

let sub z1 z2 =
  { Re = z1.Re - z2.Re
    Im = z1.Im - z2.Im }

let mul z1 z2 =
  { Re = z1.Re * z2.Re - z1.Im * z2.Im
    Im = z1.Re * z2.Im + z1.Im * z2.Re }

let div z1 z2 =
  let denom = z2.Re * z2.Re + z2.Im * z2.Im
  { Re = (z1.Re * z2.Re + z1.Im * z2.Im) / denom
    Im = (z1.Im * z2.Re - z1.Re * z2.Im) / denom }

let rec pow z n =
  if n = 0 then { Re = 1.0; Im = 0.0 }
  elif n = 1 then z
  else mul z (pow z (n - 1))

let printComplex z =
  if z.Im >= 0.0 then
    printfn "%f + %fi" z.Re z.Im
  else
    printfn "%f - %fi" z.Re (abs z.Im)



[<EntryPoint>]
let main argv =

  // Первое задание
  let listResult = first []
  printfn "\nИтоговый список: %A\n" listResult

  // Второе задание
  printf "Введите число: "
  let n = int(Console.ReadLine())
  let result = prov n
  printfn "Число: %d" n
  printfn "Четные цифры: %A\n" result

  // Третье задание
  printfn "Работа с комплексными числами"

  printf "Введите действительную часть первого числа: "
  let re1 = float(Console.ReadLine())
  printf "Введите мнимую часть первого числа: "
  let im1 = float(Console.ReadLine())
  let z1 = { Re = re1; Im = im1 }

  printf "Введите действительную часть второго числа: "
  let re2 = float(Console.ReadLine())
  printf "Введите мнимую часть второго числа: "
  let im2 = float(Console.ReadLine())
  let z2 = { Re = re2; Im = im2 }

  printf "Введите стпень для первого числа: "
  let st = int(Console.ReadLine())

  printfn "\nСумма:"
  printComplex (add z1 z2)

  printfn "Разность:"
  printComplex (sub z1 z2)

  printfn "Произведение:"
  printComplex (mul z1 z2)

  printfn "Деление:"
  printComplex (div z1 z2)

  printfn "Первое число в 3-й степени:"
  printComplex (pow z1 st)

  0
