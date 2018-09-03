import { Expression } from './expression';

export abstract class InterimExpression extends Expression {

  protected readonly name: string;

  public isValid(): boolean {
    throw Error(`${this.errorPrefix} Its validity is not known.`);
  }

  public checkTypes(): Expression {
    throw Error(`${this.errorPrefix} Its type is not known.`);
  }

  private get errorPrefix() {
    return `${this.name} is a temporary construction.`;
  }

}
