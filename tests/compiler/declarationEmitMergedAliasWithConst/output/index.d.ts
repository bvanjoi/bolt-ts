const Color: { Red: "Red"; Green: "Green"; Blue: "Blue"; };

type Color = typeof Color;
type Colors = Color[keyof Color];
