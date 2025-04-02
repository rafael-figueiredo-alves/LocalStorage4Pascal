# LocalStorage4Pascal

![Version](https://img.shields.io/badge/version-1.1.0-blue.svg)

**LocalStorage4Pascal** é uma biblioteca para Delphi e Lazarus que implementa uma função semelhante ao `LocalStorage` de navegadores, permitindo o armazenamento persistente de dados em aplicações desktop e mobile. Por que usar SQLite para guardar dados, especialmente para cache de resultados de consultas a API se podes salvar como se estivesse no Browser. Guarde Token, preferência de tema e dados não sensíveis num arquivo no formato JSON e leia seus valores de forma prática, com métodos que retornam não só string, mas também boolean, double, integer, int64, TJsonObject e TJsonArray.

## 🌟 Recursos
- Compatível com **Windows, macOS, Linux, Android e iOS**.
- Suporte para **Delphi XE2+** e **Lazarus 2+**.
- Armazenamento de dados persistente usando um sistema leve e eficiente.

## 📦 Instalação

### 🔹 Opção 1: Instalação Manual
1. Copie os arquivos da pasta `src` para o seu projeto.
2. Inclua as units no seu projeto.

### 🔹 Opção 2: Instalação com BOSS (Delphi e Lazarus)
O **BOSS** é um gerenciador de pacotes para Delphi e Lazarus.

1. Instale o **BOSS** se ainda não tiver:
   ```sh
   pip install boss-delphi
   ```
2. No terminal, dentro do diretório do seu projeto, execute:
   ```sh
   boss install https://github.com/rafael-figueiredo-alves/LocalStorage4Pascal
   ```

## 🚀 Como Usar

1. **Inicialize a biblioteca** no arquivo principal do projeto:
   - **Delphi (dpr)**:
     ```pascal
     uses LocalStorage4Pascal;
     
     begin
       InitLocalStorage4Pascal('projeto.json');
     end.
     ```
   - **Lazarus**:
     ```pascal
     uses LocalStorage4Pascal;
     
     begin
       InitLocalStorage4Pascal('projeto.json');
     end.
     ```

2. **Acesse o armazenamento local:**
   - **Delphi:**
     ```pascal
     LocalStorage4Delphi.SetValue('username', 'Rafael');
     ShowMessage(LocalStorage4Delphi.GetValue('username'));
     ```
   - **Lazarus:**
     ```pascal
     LocalStorage4Lazarus.SetValue('username', 'Rafael');
     ShowMessage(LocalStorage4Lazarus.GetValue('username'));
     ```

---

# LocalStorage4Pascal (English)

![Version](https://img.shields.io/badge/version-1.1.0-blue.svg)

**LocalStorage4Pascal** is a library for Delphi and Lazarus that implements a `LocalStorage`-like feature, allowing persistent data storage in desktop and mobile applications. Why use SQLite to store data, especially for caching API query results, if you can save it as if it were in the browser. Store Token, theme preference and non-sensitive data in a JSON file and read their values ​​conveniently, with methods that return not only string, but also boolean, double, integer, int64, TJsonObject and TJsonArray.

## 🌟 Features
- Compatible with **Windows, macOS, Linux, Android, and iOS**.
- Supports **Delphi XE2+** and **Lazarus 2+**.
- Lightweight and efficient persistent data storage.

## 📦 Installation

### 🔹 Option 1: Manual Installation
1. Copy the `src` folder files into your project.
2. Include the units in your project.

### 🔹 Option 2: Install with BOSS (Delphi & Lazarus)
**BOSS** is a package manager for Delphi and Lazarus.

1. Install **BOSS** if you haven't already:
   ```sh
   pip install boss-delphi
   ```
2. Inside your project directory, run:
   ```sh
   boss install https://github.com/rafael-figueiredo-alves/LocalStorage4Pascal
   ```

## 🚀 How to Use

1. **Initialize the library** in the main project file:
   - **Delphi (dpr)**:
     ```pascal
     uses LocalStorage4Pascal;
     
     begin
       InitLocalStorage4Pascal('projeto.json');
     end.
     ```
   - **Lazarus**:
     ```pascal
     uses LocalStorage4Pascal;
     
     begin
       InitLocalStorage4Pascal('projeto.json');
     end.
     ```

2. **Access local storage:**
   - **Delphi:**
     ```pascal
     LocalStorage4Delphi.SetValue('username', 'Rafael');
     ShowMessage(LocalStorage4Delphi.GetValue('username'));
     ```
   - **Lazarus:**
     ```pascal
     LocalStorage4Lazarus.SetValue('username', 'Rafael');
     ShowMessage(LocalStorage4Lazarus.GetValue('username'));
     

