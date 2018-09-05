import { ValueType } from '../../../common/model';
import { TermExpression } from './term';

export class TextExpression extends TermExpression<string> {

  public static fromTerm({ value }: TermExpression) {
    return TextExpression.of(value);
  }

  public static of(value: string) {
    return new TextExpression(value);
  }

  public readonly returnType: ValueType = ValueType.Text;

  constructor(value: string) {
    super(value, value.trim());
  }

}
