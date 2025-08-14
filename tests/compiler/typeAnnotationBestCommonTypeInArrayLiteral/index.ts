// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeAnnotationBestCommonTypeInArrayLiteral.ts`, Apache-2.0 License

interface IMenuItem {
  id: string;
  type: string;
  link?: string;
  classes?: string;
  text?: string;
  icon?: string;
}
var menuData: IMenuItem[] = [
  {
      "id": "ourLogo",
      "type": "image",
      "link": "",
      "icon": "modules/menu/logo.svg"
  }, {
      "id": "productName",
      "type": "default",
      "link": "",
      "text": "Product Name"
  }
];
