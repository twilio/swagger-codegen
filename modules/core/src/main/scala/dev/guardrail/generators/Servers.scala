package dev.guardrail.generators

import dev.guardrail.core.SupportDefinition
import dev.guardrail.languages.LA

case class Servers[L <: LA](servers: List[Server[L]], supportDefinitions: List[SupportDefinition[L]])
case class Server[L <: LA](pkg: List[String], extraImports: List[L#Import], handlerDefinition: L#Definition, serverDefinitions: List[L#Definition])

//todo: add description
case class CustomExtractionField[L <: LA](param: LanguageParameter[L], term: L#Term)
case class TracingField[L <: LA](param: LanguageParameter[L], term: L#Term)
case class RenderedRoutes[L <: LA](
    routes: List[L#Statement],
    classAnnotations: List[L#Annotation],
    methodSigs: List[L#MethodDeclaration],
    supportDefinitions: List[L#Definition],
    handlerDefinitions: List[L#Statement],
    securitySchemesDefinitions: List[L#Definition]
)
