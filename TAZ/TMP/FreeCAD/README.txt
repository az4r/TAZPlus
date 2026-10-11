################
### TUTORIAL ###
################

1. Import external reference model to GstarCAD (.dwg or another format).
2. Load and use Use taz_explode_blocks.lsp script.
3. Export xref to IFC2X3.
4. Import IFC to FreeCAD (IMPORTANT: Change representation type to: Load the shape (slower))
6. Switch active workbench to: BIM
5. Set in Preferences in FreeCAD:
		Import-Export -> IFC Export -> Check: Use DAE triangulation option
		Import-Export -> IFC Export -> Check: Join coplanar facets when triangulating
		Import-Export -> DAE -> Tessellation: 0,0100
		BIM -> General Settings -> IFC version: IFC2X3
5. Run IFC2SOLIDS.FCMacro in FreeCAD.
6. Import exported from FreeCAD IFC model to GstarCAD.
