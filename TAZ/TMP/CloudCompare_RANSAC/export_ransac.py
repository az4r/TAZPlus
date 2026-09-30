import pycc

cc = pycc.GetInstance()
db = cc.dbRootObject()

# Znajdź grupę "Ransac Detected Shapes"
ransac = None

for i in range(db.getChildrenNumber()):
    obj = db.getChild(i)
    if obj.getName().startswith("Ransac Detected Shapes"):
        ransac = obj
        break

if ransac is None:
    raise RuntimeError("Nie znaleziono grupy 'Ransac Detected Shapes'.")

# Plik wynikowy
output_file = r"C:\CloudCompare_RANSAC\ransac_export.txt"

# Zamiana CCVector3 na zwykłą krotkę liczb
def vec(v):
    return (float(v.x), float(v.y), float(v.z))

lines = []

# Przejdź po wszystkich wykrytych obiektach
for i in range(ransac.getChildrenNumber()):

    parent = ransac.getChild(i)

    # Pomijamy Leftovers
    if parent.getName() == "Leftovers":
        continue

    # Każdy wykryty prymityw ma właściwy obiekt jako pierwsze dziecko
    if parent.getChildrenNumber() == 0:
        continue

    primitive = parent.getChild(0)
    primitive_type = primitive.getTypeName()
    name = parent.getName()

    # Transformacja prymitywu
    history = primitive.getGLTransformationHistory()

    center = vec(history.getTranslationAsVec3D())
    axis = vec(history.getColumnAsVec3D(2))

    # Cylinder
    if primitive_type == "Cylinder":

        radius = float(primitive.getRadius())
        height = float(primitive.getHeight())

        lines.append(
            f"CYLINDER|{name}|{radius}|{height}|"
            f"{center[0]}|{center[1]}|{center[2]}|"
            f"{axis[0]}|{axis[1]}|{axis[2]}"
        )

    # Sphere
    elif primitive_type == "Sphere":

        radius = float(primitive.getRadius())

        lines.append(
            f"SPHERE|{name}|{radius}|"
            f"{center[0]}|{center[1]}|{center[2]}"
        )

    # Cone
    elif primitive_type == "Cone":

        bottom_radius = float(primitive.getBottomRadius())
        top_radius = float(primitive.getTopRadius())
        height = float(primitive.getHeight())

        lines.append(
            f"CONE|{name}|{bottom_radius}|{top_radius}|{height}|"
            f"{center[0]}|{center[1]}|{center[2]}|"
            f"{axis[0]}|{axis[1]}|{axis[2]}"
        )

    # Plane
    elif primitive_type == "Plane":

        lines.append(
            f"PLANE|{name}|"
            f"{center[0]}|{center[1]}|{center[2]}|"
            f"{axis[0]}|{axis[1]}|{axis[2]}"
        )

# Zapis TXT
with open(output_file, "w") as f:
    f.write("\n".join(lines))

print("========================================")
print("RANSAC EXPORT ZAKONCZONY")
print("Liczba obiektow:", len(lines))
print("Plik:", output_file)
print("========================================")