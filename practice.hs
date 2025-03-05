import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    ide :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún está en la universidad o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante a la universidad
registrarEntrada :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada idEstudiante tiempo universidad =
    if length idEstudiante >= 7 && length idEstudiante <= 12
    then Estudiante idEstudiante tiempo Nothing : universidad
    else error "El ID del estudiante debe tener entre 7 y 11 caracteres y ser numérico"

-- Función para registrar la salida de un estudiante de la universidad
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida ideEstudiante tiempo = map (\v -> if ideEstudiante == ide v then v { salida = Just tiempo } else v)

-- Función para buscar un estudiante por su ID en la universidad
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante ideEstudiante = find (\v -> ideEstudiante == ide v && isNothing (salida v))
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un estudiante permaneció en la universidad
tiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (entrada estudiante)

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarUniversidad :: [Estudiante] -> IO ()
guardarUniversidad universidad = do
    withFile "universidad.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante universidad))
    putStrLn "Universidad guardada en el archivo universidad.txt."

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarUniversidad :: IO [Estudiante]
cargarUniversidad = do
    contenido <- withFile "universidad.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante id entrada salida) =
    "Estudiante {id = \"" ++ id ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"

-- Función para listar los estudiantes en la universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes en la universidad."
listarEstudiantes estudiantes = do
    putStrLn "Estudiantes en la universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar la universidad desde el archivo de texto
    universidad <- catch cargarUniversidad manejarError
    putStrLn "¡Bienvenido al Sistema de Gestión de la Universidad!"
    -- Ciclo principal del programa
    cicloPrincipal universidad
  where
    manejarError :: IOException -> IO [Estudiante]
    manejarError _ = do
        putStrLn "No se pudo cargar el archivo universidad.txt. Iniciando con una universidad vacía."
        return []

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Editar información de estudiantes"
    putStrLn "6. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            ideEstudiante <- getLine
            if length ideEstudiante >= 7 && length ideEstudiante <= 11 
                then do
                    tiempoActual <- getCurrentTime
                    let universidadActualizado = registrarEntrada ideEstudiante tiempoActual universidad
                    putStrLn $ "Estudiante con ID " ++ ideEstudiante ++ " ingresado a la Universidad."
                    catch (guardarUniversidad universidadActualizado) manejarErrorGuardar
                    cicloPrincipal universidadActualizado
                else do
                    putStrLn "ID no válido. Debe tener entre 7 y 11 caracteres y ser numérico."
                    cicloPrincipal universidad

        "2" -> do
            putStrLn "Ingrese el ID del estudiante a salir:"
            ideEstudiante <- getLine
            if length ideEstudiante >= 7 && length ideEstudiante <= 11
                then do
                    tiempoActual <- getCurrentTime
                    let universidadActualizado = registrarSalida ideEstudiante tiempoActual universidad
                    putStrLn $ "Estudiante con el ID " ++ ideEstudiante ++ " salido de la Universidad."
                    catch (guardarUniversidad universidadActualizado) manejarErrorGuardar
                    cicloPrincipal universidadActualizado
                else do
                    putStrLn "ID no válido. Debe tener entre 7 y 11 caracteres y ser numérico."
                    cicloPrincipal universidad


        "3" -> do
            putStrLn "Ingrese el ID del Estudiante a buscar:"
            ideEstudiante <- getLine
            if length ideEstudiante >= 7 && length ideEstudiante <= 11
                then do
                    case buscarEstudiante ideEstudiante universidad of
                        Just estudiante -> do
                            tiempoTotal <- tiempoEnUniversidad estudiante
                            putStrLn $ "El estudiante con ID " ++ ideEstudiante ++ " se encuentra en la Universidad."
                            putStrLn $ "Tiempo en la Universidad: " ++ show tiempoTotal ++ " segundos."
                        Nothing -> putStrLn "Estudiante no encontrado en la universidad."
                    cicloPrincipal universidad
                else do
                    putStrLn "ID no válido. Debe tener entre 7 y 11 caracteres y ser numérico."
                    cicloPrincipal universidad

        "4" -> do
            listarEstudiantes universidad
            cicloPrincipal universidad

        "5" -> do
            listarEstudiantes universidad
            putStrLn "Ingrese el ID del estudiante a corregir:"
            ideCorregir <- getLine
            if length ideCorregir >= 7 && length ideCorregir <= 11 
                then do
                    case buscarEstudiante ideCorregir universidad of
                        Just estudiante -> do
                            putStrLn "Ingrese el nuevo ID del estudiante:"
                            nuevoIde <- getLine
                            if length nuevoIde >= 7 && length nuevoIde <= 11
                                then do
                                    tiempoActual <- getCurrentTime
                                    let universidadActualizado = registrarSalida ideCorregir tiempoActual (registrarEntrada nuevoIde tiempoActual (filter (\v -> ideCorregir /= ide v) universidad))
                                    putStrLn $ "Estudiante de ID " ++ ideCorregir ++ " corregido exitosamente."
                                    catch (guardarUniversidad universidadActualizado) manejarErrorGuardar
                                    cicloPrincipal universidadActualizado
                                else do
                                    putStrLn "Nuevo ID no válido. Debe tener entre 7 y 11 caracteres y ser numérico."
                                    cicloPrincipal universidad
                        Nothing -> do
                            putStrLn "Estudiante no encontrado en la universidad."
                            cicloPrincipal universidad
                else do
                    putStrLn "ID no válido. Debe tener entre 7 y 11 caracteres y ser numérico."
                    cicloPrincipal universidad

        "6" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione u+na opción válida."
            cicloPrincipal universidad
  where
    manejarErrorGuardar :: IOException -> IO ()
    manejarErrorGuardar _ = putStrLn "Error al guardar la información de la universidad."