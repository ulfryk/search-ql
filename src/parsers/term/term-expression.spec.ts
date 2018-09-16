/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { zip } from 'lodash';

import { NumberExpression, TermExpression, TextExpression } from '../../ast';
import { ParserConfig } from '../../config';
import { num, txt } from '../../testing/utils';
import { termExpression } from './term-expression';

const config = new ParserConfig();
const { AND, EXACT_MATCHER, GROUP_END, GROUP_START, OR } = config;

describe('SearchQL parsers', () => {

  const validInput = [
    '"ASDfas 32%@$%4512 u954anna as d][;];.{P} AND"',
    '"OR AND NOT (OR AND NOT) asd: asd not ASD:ASd"',
    '"  "',
    'asdANDas_NOTallalal',
    'shgfghjfhjfghs',
    '123123123',
    's#$%^876gsbjh_-s-S-s',
  ];

  const validOutput = [
    txt('ASDfas 32%@$%4512 u954anna as d][;];.{P} AND'),
    txt('OR AND NOT (OR AND NOT) asd: asd not ASD:ASd'),
    txt('  '),
    txt(validInput[3]),
    txt(validInput[4]),
    num(validInput[5]),
    txt(validInput[6]),
  ];

  const invalidInput = [
    'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
    'OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
    ' ',
    ...AND,
    ...OR,
    `123 ~= asd`,
    `asd : 23`,
    `${GROUP_START}asd${GROUP_END}`,
    `dasd${EXACT_MATCHER}a`,
  ];

  describe('termExpression', () => {

    zip<any>(validInput, validOutput)
      .forEach(([input, output], index) => {
        describe(`for valid input: '${input}'`, () => {
          const parsed = termExpression(config).parse(input);

          it('should succeed', () => {
            expect(parsed.status).to.be.true;
          });

          it('should be evaluated to proper expression type', () => {
            expect(parsed.status ? parsed.value : null).to.be.instanceOf(TermExpression);
            expect(parsed.status ? parsed.value : null)
              .to.be.instanceOf(index === 5 ? NumberExpression : TextExpression);
          });

          it('should provide proper value', () => {
            expect(parsed.status ? parsed.value : null).to.deep.equal(output);
          });

        });
      });

    invalidInput.forEach(input => {
      describe(`for invalid input: '${input}'`, () => {

        it('should fail', () => {
          expect(termExpression(config).parse(input).status).to.be.false;
        });

      });
    });

  });

});
