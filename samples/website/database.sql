
-- Execute this file on database cobolscriptwebsite

--
-- Table structure for table `customers`
--

DROP TABLE IF EXISTS `customers`;
CREATE TABLE IF NOT EXISTS `customers` (
  `Id` int(11) NOT NULL AUTO_INCREMENT,
  `Name` varchar(200) DEFAULT NULL,
  `Address` text,
  `Notes` text,
  PRIMARY KEY (`Id`)
) TYPE=InnoDB;

--
-- Dumping data for table `customers`
--

INSERT INTO `customers` (`Id`, `Name`, `Address`, `Notes`) VALUES
(1, 'Google', 'Address', 'Notes');
INSERT INTO `customers` (`Id`, `Name`, `Address`, `Notes`) VALUES
(2, 'Apple', 'Address', 'Notes');
INSERT INTO `customers` (`Id`, `Name`, `Address`, `Notes`) VALUES
(3, 'Microsoft', 'Address', 'Notes');

-- --------------------------------------------------------

--
-- Table structure for table `suppliers`
--

DROP TABLE IF EXISTS `suppliers`;
CREATE TABLE IF NOT EXISTS `suppliers` (
  `Id` int(11) NOT NULL AUTO_INCREMENT,
  `Name` varchar(200) DEFAULT NULL,
  `Address` text,
  `Notes` text,
  PRIMARY KEY (`Id`)
) TYPE=InnoDB;

INSERT INTO `suppliers` (`Id`, `Name`, `Address`, `Notes`) VALUES
(1, 'Oracle', 'Address', 'Notes');
INSERT INTO `suppliers` (`Id`, `Name`, `Address`, `Notes`) VALUES
(2, 'SAP', 'Address', 'Notes');
INSERT INTO `suppliers` (`Id`, `Name`, `Address`, `Notes`) VALUES
(3, 'Salesforce', 'Address', 'Notes');
