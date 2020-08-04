module Tct.Common.CeTA where


import qualified Tct.Core.Common.Xml as Xml


cetaDocument :: Xml.XmlContent -> Xml.XmlDocument
cetaDocument = Xml.toDocument (Just css)
  where css = "<?xml-stylesheet type=\"text/xsl\" href=\"cpfHTML.xsl\"?>"

certificationProblem :: Xml.XmlContent -> Xml.XmlContent -> Xml.XmlContent
certificationProblem complexityInput' complexityProof' = 
  flip Xml.setAtts [att1, att2] $ Xml.elt "certificationProblem" [input, cpfVersion, proof, origin]
  where
    att1 = Xml.att "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance"
    att2 = Xml.att "xsi:noNamespaceSchemaLocation" "cpf.xsd"

    input = Xml.elt "input" [complexityInput']

    proof = Xml.elt "proof" [complexityProof']

    cpfVersion = Xml.elt "cpfVersion" [Xml.text "2.2"]

    origin = Xml.elt "origin" [proofOrigin]
      where
        proofOrigin = Xml.elt "proofOrigin" [tool]
        tool    = Xml.elt "tool" [name, version]
        name    = Xml.elt "name" [Xml.text "TcT"]
        version = Xml.elt "version" []


complexityInput :: Xml.XmlContent -> Xml.XmlContent -> Xml.XmlContent -> Xml.XmlContent
complexityInput trsInput' complexityMeasure' complexityClass' = 
  Xml.elt "complexityInput" [trsInput', complexityMeasure', complexityClass']

trsInput :: Xml.XmlContent -> Xml.XmlContent -> Xml.XmlContent -> Xml.XmlContent
trsInput trs' strategy' relativeRules' = Xml.elt "trsInput" [trs', strategy', relativeRules']

complexityProof :: Xml.XmlContent -> Xml.XmlContent
complexityProof complexityProof' = Xml.elt "complexityProof" [complexityProof']

unknownProof :: String -> Xml.XmlContent -> [Xml.XmlContent] -> Xml.XmlContent
unknownProof desc complexityInput' subProofs' = Xml.elt "unknownProof" $ desce :complexityInput'  :subProofs'
  where desce = Xml.elt "description" [Xml.text desc]

subProof :: Xml.XmlContent -> Xml.XmlContent -> Xml.XmlContent
subProof complexityInput' complexityProof' = Xml.elt "subProof" [complexityInput', complexityProof']

