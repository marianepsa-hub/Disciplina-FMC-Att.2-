#install.packages("stringr")
#install.packages("openxlsx")

library(pdftools)
library(stringr)

arquivo <- "./cadastro.pdf"

conteudo <- pdftools::pdf_text(arquivo)
conteudo[1]

# quebrando as linhas
conteudo <- str_split(conteudo, "\\n")

# transformando a lista em linhas

conteudo[[1]]
#ou
linhas <- conteudo[[1]]
linhas

# encontrando o início de cada cadastro
inicio <- grep("^Nome:", linhas, ignore.case = TRUE)

# encontrando o fim de cada cadastro
fim <- c(inicio[-1] - 1, length(linhas))


#Criando uma lsita para guardar os cadastros

dados <- list()


#Para percorrer por cada pessoa

for (i in seq_along(inicio)) {

  # Seleciona somente as linhas daquela pessoa
  cadastro <- linhas[inicio[i]:fim[i]]



  # Nome

  linha_nome <- cadastro[
    str_detect(
      cadastro,
      regex("^Nome:", ignore_case = TRUE)
    )
  ]

  nome <- str_remove(
    linha_nome,
    regex("^Nome:\\s*", ignore_case = TRUE)
  )


  
  # Data de nascimento

  linha_nascimento <- cadastro[
    str_detect(
      cadastro,
      regex(
        "^(Data de nascimento|Dt nasc):",
        ignore_case = TRUE
      )
    )
  ]

  nascimento <- str_remove(
    linha_nascimento,
    regex(
      "^(Data de nascimento|Dt nasc):\\s*",
      ignore_case = TRUE
    )
  )



  # Endereço

  linha_endereco <- cadastro[
    str_detect(
      cadastro,
      regex("^Endereço:", ignore_case = TRUE)
    )
  ]

  endereco <- str_remove(
    linha_endereco,
    regex("^Endereço:\\s*", ignore_case = TRUE)
  )



  # Se o CEP estiver na mesma linha do endereço,
  # remove o trecho "CEP: ..."
  endereco <- str_remove(
    endereco,
    regex("\\s*CEP:.*$", ignore_case = TRUE)
  )


  # CEP

  linha_cep <- cadastro[
    str_detect(
      cadastro,
      regex("CEP:", ignore_case = TRUE)
    )
  ]

  cep <- str_extract(
    linha_cep,
    "\\d{2,3}[.\\-]?\\d{3}[.\\-]?\\d{3}"
  )



  # Telefone

  linha_tel <- cadastro[
    str_detect(
      cadastro,
      regex("^(Tel|Telefone):", ignore_case = TRUE)
    )
  ]

  telefone <- str_remove(
    linha_tel,
    regex("^(Tel|Telefone):\\s*", ignore_case = TRUE)
  )


  # CPF

  linha_cpf <- cadastro[
    str_detect(
      cadastro,
      regex("^CPF:", ignore_case = TRUE)
    )
  ]

  cpf <- str_remove(
    linha_cpf,
    regex("^CPF:\\s*", ignore_case = TRUE)
  )


  # Guardando os dados das pessoas

  dados[[i]] <- data.frame(
    Nome = nome,
    `Data de nascimento` = nascimento,
    Endereço = endereco,
    CEP = cep,
    Telefone = telefone,
    CPF = cpf,
    stringsAsFactors = FALSE
  )
}

# Juntando todos os cadastros em um único data frame pra ficar bonito

dados <- do.call(rbind, dados)


# Verificando o resultado

dados


# Gerar um arquivo Excel com os dados extraídos

openxlsx::write.xlsx(
  dados,
  "./resultado_Att2_extracao.xlsx"
)