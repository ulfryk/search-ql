import { TermExpression } from './term';

export class TextExpression extends TermExpression<string> {

  public static fromTerm({ value }: TermExpression) {
    return TextExpression.of(value);
  }

  public static of(value: string) {
    return new TextExpression(value);
  }

  constructor(value: string) {
    super(value, value.trim());
  }

}
