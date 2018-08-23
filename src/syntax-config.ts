export class SyntaxConfig {

  public static create(
    { AND, NOT, OR, GROUP_START, GROUP_END, EXACT_MATCHER, LABEL_DELIMITER }: Partial<SyntaxConfig>,
  ) {
    return new SyntaxConfig(AND, NOT, OR, GROUP_START, GROUP_END, EXACT_MATCHER, LABEL_DELIMITER);
  }

  constructor(
    public readonly AND = 'AND',
    public readonly NOT = 'NOT',
    public readonly OR = 'OR',
    public readonly GROUP_START = '(',
    public readonly GROUP_END = ')',
    public readonly EXACT_MATCHER = '"',
    public readonly LABEL_DELIMITER = ':',
  ) {}

  public get restricted() {
    return [this.EXACT_MATCHER, this.GROUP_END, this.GROUP_START, this.LABEL_DELIMITER, 's']
      .map(sign => `\\${sign}`)
      .join('');
  }

  public get restrictedSigns() {
    return new RegExp(`^[^${this.restricted}]+`);
  }

  public get word() {
    return /^[\w_\.]+/;
  }

  public get delimiter() {
    return new RegExp(`\\s*${this.LABEL_DELIMITER}\\s*`);
  }

}
