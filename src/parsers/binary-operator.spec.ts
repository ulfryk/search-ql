/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import * as P from 'parsimmon';

import { ParserConfig } from '../config';
import { binaryOperator } from './binary-operator';

const config = new ParserConfig();
const { AND, IS, IS_NOT, LIKE, OR } = config;

describe('SearchQL parsers', () => {

  describe('binaryOperator', () => {

    const validInput = [...AND, ...IS, ...IS_NOT, ...LIKE, ...OR];

    validInput.forEach(input => {
      describe(`for '${input}'`, () => {
        const parsed = binaryOperator(config).parse(input);

        it('should succeed', () => {
          expect(parsed.status).to.be.true;
        });

        it('should provide proper value', () => {
          expect(parsed.status ? parsed.value : null).to.equal(input);
        });

      });
    });

    ['NOR', 'XOR', 'NAND'].forEach(input => {
      describe(`for '${input}'`, () => {

        it('should fail', () => {
          expect(binaryOperator(config).parse(input).status).to.be.false;
        });

      });
    });

    describe('for many valid occurrences', () => {
      const input = validInput.join(' ');
      const parser = P.sepBy1(binaryOperator(config), P.whitespace);
      const expectedOutput = validInput;
      const output = parser.parse(input);

      it('should succeed', () => {
        expect(output.status).to.be.true;
      });

      it('should provide proper value', () => {
        expect(output.status ? output.value : null).to.deep.equal(expectedOutput);
      });

    });

    describe('for mixed input (valid occurrences mixed with invalid strings)', () => {
      const input = validInput.concat(['NOR', 'XOR', 'NAND']).join(' ');
      const parser = P.sepBy1(binaryOperator(config), P.whitespace);
      const output = parser.parse(input);

      it('should fail', () => {
        expect(output.status).to.be.false;
      });

    });

  });

});
