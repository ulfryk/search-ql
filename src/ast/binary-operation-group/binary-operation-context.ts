import { Ordering } from '../../common/model';
import { BinaryOperationExpression, Expression } from '../expressions';
import { OperatorContext } from './operator-context';

export class BinaryOperationContext extends Expression {

  constructor(
    public readonly value: OperatorContext,
    public readonly left: Expression,
    public readonly right: Expression,
  ) { super(); }

  // tslint:disable-next-line:cyclomatic-complexity
  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof BinaryOperationContext &&
      this.value.equals(other.value) &&
      this.left.equals(other.left) &&
      this.right.equals(other.right)
    );
  }

  public toString() {
    return `( ${this.left} ${this.value} ${this.right} )`;
  }

  public rebuild(): BinaryOperationExpression {
    return BinaryOperationExpression.fromPair(this.value.operator)(
      this.left.rebuild(),
      this.right.rebuild());
  }

  public append(operator: OperatorContext, rhs: Expression) {
    switch (this.value.compare(operator)) {
      case (Ordering.Eq):
      case (Ordering.Gt):
        return new BinaryOperationContext(operator, this, rhs);

      case (Ordering.Lt):
        return new BinaryOperationContext(
          this.value,
          this.left,
          new BinaryOperationContext(operator, this.right, rhs));

      default:
        throw Error(`There's no such Ordering value: "${this.value.compare(operator)}"`);
    }
  }

}
