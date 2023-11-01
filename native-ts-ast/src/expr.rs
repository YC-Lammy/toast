pub use pegy::Parse;
use pegy::Span;
use pegy::util::{
    ANY,
    Repeat,
    Recursive
};

use crate::utils::*;

#[derive(Debug, Parse)]
pub enum IdentifierReference{
    /// identifier
    #[grammar($item0: Identifier)]
    Identifier(Identifier),
    /// yield
    #[grammar($item0:("yield"))]
    Yield(Span),
    /// await
    #[grammar($item0:("await"))]
    Await(Span),
}

impl Default for IdentifierReference{
    fn default() -> Self {
        Self::Identifier(Identifier::default())
    }
}

#[derive(Debug, Parse)]
pub enum BindingIdentifier{
    /// identifier
    #[grammar($item0: Identifier)]
    Identifier(Identifier),
    /// yield
    #[grammar($item0:("yield"))]
    Yield(Span),
    /// await
    #[grammar($item0:("await"))]
    Await(Span),
}

impl Default for BindingIdentifier{
    fn default() -> Self {
        Self::Identifier(Identifier::default())
    }
}

#[derive(Debug, Parse)]
pub enum LabelIdentifier{
    /// identifier
    #[grammar($item0: Identifier)]
    Identifier(Identifier),
    /// yield
    #[grammar($item0:("yield"))]
    Yield(Span),
    /// await
    #[grammar($item0:("await"))]
    Await(Span),
}

impl Default for LabelIdentifier{
    fn default() -> Self {
        Self::Identifier(Identifier::default())
    }
}


#[derive(Debug, Default, Parse)]
#[grammar(!ReservedWord $span:(IdentifierName))]
pub struct Identifier{
    pub span: Span
}

#[derive(Debug, Parse)]
pub enum PrimaryExpression{
    // this
    #[grammar($item0:("this"))]
    This(Span),
    #[grammar($item0:("super"))]
    Supar(Span),
    #[grammar($item0:IdentifierReference)]
    IdentifierReference(IdentifierReference),
    #[grammar($item0: Literal)]
    Literal(Literal),
    #[grammar($item0: ArrayLiteral)]
    ArrayLiteral(ArrayLiteral),
    #[grammar($item0: ObjectLiteral)]
    ObjectLiteral(ObjectLiteral),
    FunctionExpression(),
    GeneratorExpression(),
    AsyncFunctionExpression(),
    AsyncGeneratorExpression(),
    ClassExpression(),
    #[grammar('/' $item0:(("\\/" | !'/' ANY)*) '/' $flags:(['a'-'z''A'-'Z']*))]
    RegularExpressionLiteral{
        lit: Span,
        flags: Span
    },
    TemplateLiteral(TemplateLiteral),
    #[grammar($item0:CoverParanthesizedExpressionAndArrowParameterList)]
    CoverParanthesizedExpressionAndArrowParameterList(CoverParanthesizedExpressionAndArrowParameterList),
    #[grammar($item0:MetaProp)]
    MetaProp(MetaProp)
}

impl Default for PrimaryExpression{
    fn default() -> Self {
        Self::This(Span::default())
    }
}

#[derive(Debug, Parse)]
pub enum CoverParanthesizedExpressionAndArrowParameterList{
    #[grammar($span:("(" ")"))]
    Empty{
        span: Span,
    },
    #[grammar("(" $item0: Expression ","? ")")]
    Expression(Expression),
    #[grammar("(" "..." $item0:BindingIdentifier ")")]
    BindingIdentifier(BindingIdentifier),
    #[grammar("(" $item0: Expression "," "..." $item1:BindingIdentifier ")")]
    ExpressionBindingIdentifier(Expression, BindingIdentifier),
    #[grammar("(" "..." $item0:BindingPattern ")")]
    BindingPattern(BindingPattern),
    #[grammar("(" $expr:Expression "," "..." $binding:BindingPattern ")")]
    ExpressionBindingPattern{
        expr:Expression, 
        binding: BindingPattern
    },
}

impl Default for CoverParanthesizedExpressionAndArrowParameterList{
    fn default() -> Self {
        Self::Empty{
            span: Span::default()
        }
    }
}

#[derive(Debug, Parse)]
pub enum Literal{
    #[grammar($item0:("null"))]
    Null(Span),
    #[grammar($item0:("true"))]
    True(Span),
    #[grammar($item0:("false"))]
    False(Span),
    #[grammar($item0: NumericLiteral)]
    Number(NumericLiteral),
    /// span of the non parsed string
    #[grammar($item0:StringLiteral)]
    String(StringLiteral),
}

impl Default for Literal{
    fn default() -> Self {
        Literal::Null(Span::default())
    }
}

#[derive(Debug, Default, Parse)]
#[grammar($span:("[" $elements:ArrayElementList "]"))]
pub struct ArrayLiteral{
    pub span: Span,
    pub elements: Vec<Option<ArrayLiteralElement>>,
}

type ArrayElementList = Repeat<Option<ArrayLiteralElement>, 0, {usize::MAX},{',' as u32}>;

#[derive(Debug, Default, Parse)]
#[grammar($spread:("...")? $expr: AssignmentExpr)]
pub struct ArrayLiteralElement{
    pub spread: Option<Span>,
    pub expr: AssignmentExpr
}

#[derive(Debug, Default, Parse)]
#[grammar("{" $properties:PropertyDefinitionList "}")]
pub struct ObjectLiteral{
    pub span: Span,
    pub properties: Vec<PropertyDefinition>,
}

type PropertyDefinitionList = Repeat<PropertyDefinition, 0, {usize::MAX}, {',' as u32}>;

#[derive(Debug, Parse)]
pub enum PropertyDefinition{
    #[grammar($ident: IdentifierReference ('=' $init: AssignmentExpr)?)]
    Identifier{
        ident: IdentifierReference,
        init: Option<AssignmentExpr>,
    },
    #[grammar($name:PropertyName ':' $value:AssignmentExpr)]
    Property{
        name: PropertyName,
        value: AssignmentExpr
    },
    #[grammar($item0: MethodDefinition)]
    Method(MethodDefinition),
    #[grammar($spread:("...") $expr:AssignmentExpr)]
    Spread{
        spread: Span,
        expr: AssignmentExpr
    }
}

impl Default for PropertyDefinition{
    fn default() -> Self {
        Self::Identifier { ident: Default::default(), init: None }
    }
}

#[derive(Debug, Parse)]
pub enum PropertyName{
    #[grammar($item0: IdentifierName)]
    Identifier(IdentifierName),
    #[grammar($item0: StringLiteral)]
    String(StringLiteral),
    #[grammar($item0: NumericLiteral)]
    Numeric(NumericLiteral),
    #[grammar($span:('[' $expr:AssignmentExpr ']'))]
    Computed{
        span: Span,
        expr: AssignmentExpr
    }
}

impl Default for PropertyName{
    fn default() -> Self {
        PropertyName::Identifier(IdentifierName(Span::default()))
    }
}

#[derive(Debug, Default, Parse)]
#[grammar($span:('`' $elements:TemplateElem* '`'))]
pub struct TemplateLiteral{
    pub span: Span,
    pub elements: Vec<TemplateElem>
}

#[derive(Debug, Parse)]
pub enum TemplateElem{
    #[grammar($item0:(("\\`" | !"`" !"${" ANY)* ))]
    String(Span),
    #[grammar("${" $item0:Expression "}")]
    Expr(Expression),
}

impl Default for TemplateElem{
    fn default() -> Self {
        TemplateElem::String(Span::default())
    }
}

#[derive(Debug, Default, Parse)]
pub enum MetaProp{
    #[default]
    #[grammar("new" "." "target")]
    NewTarget,
    #[grammar("import" "." "meta")]
    ImortMeta,
}

#[derive(Debug, Parse)]
pub enum MemberExpression{
    #[grammar($item0: PrimaryExpression)]
    PrimaryExpression(PrimaryExpression),
    #[grammar($obj: Recursive<MemberExpression> '[' $prop:Expression ']')]
    ComputedMember{
        obj: Box<MemberExpression>,
        prop: Expression 
    },
    #[grammar($obj: Recursive<MemberExpression> '.' $prop:IdentifierName)]
    Member{
        obj: Box<MemberExpression>,
        prop: IdentifierName,
    },
    #[grammar($obj: Recursive<MemberExpression> $template:TemplateLiteral)]
    TaggedTemplate{
        tag: Box<MemberExpression>,
        template: TemplateLiteral,
    },
    #[grammar("super" '[' $prop:Expression ']')]
    SuperComputedProp{
        prop: Expression
    },
    #[grammar("super" '.' $prop:IdentifierName)]
    SuperProp{
        prop: IdentifierName
    },
    #[grammar($item0: MetaProp)]
    MetaProp(MetaProp),
    #[grammar("new" $target:Recursive<MemberExpression> $args:Arguments)]
    NewExpr{
        target: Box<MemberExpression>,
        args: Arguments
    },
    #[grammar($obj:Recursive<MemberExpression> '.' '#' $prop:IdentifierName)]
    PrivateProp{
        obj: Box<MemberExpression>,
        prop: IdentifierName
    }
}



#[derive(Debug, Parse)]
pub enum NewExpression{
    #[grammar($item0:MemberExpression)]
    MemberExpr(MemberExpression),
    #[grammar("new" $item0: Recursive<NewExpression>)]
    NewExpr(Box<NewExpression>),
}

#[derive(Debug, Parse)]
pub enum CallExpr{
    #[grammar($item0:CoverCallExprAndAsyncArrowHead)]
    CoverCallExprAndAsyncArrowHead(CoverCallExprAndAsyncArrowHead),
    #[grammar("super" $args:Arguments)]
    SuperCall{
        args: Arguments
    },
    #[grammar("import" '(' $arg: AssignmentExpr ')')]
    ImportCall{
        arg: AssignmentExpr
    },
    #[grammar($call:Recursive<CallExpr> $args:Arguments)]
    CallArgs{
        call: Box<CallExpr>,
        args: Arguments,
    },
    #[grammar($call:Recursive<CallExpr> '[' $prop:Expression ']')]
    ComputedProp{
        call: Box<CallExpr>,
        prop: Expression,
    },
    #[grammar($call:Recursive<CallExpr> '.' $prop:IdentifierName)]
    Prop{
        call: Box<CallExpr>,
        prop: IdentifierName
    },
    #[grammar($call:Recursive<CallExpr> $template:TemplateLiteral)]
    TaggedTemplate{
        call: Box<CallExpr>,
        template: TemplateLiteral
    },
    #[grammar($call:Recursive<CallExpr> '.' '#' $prop: IdentifierName)]
    PrivateProp{
        call: Box<CallExpr>,
        prop: IdentifierName
    }
}

#[derive(Debug, Default, Parse)]
#[grammar('(' $args:ArgumentList ')')]
pub struct Arguments{
    pub args: Vec<Argument>,
}

type ArgumentList = Repeat<Argument, 0, {usize::MAX}, {',' as u32}>;

#[derive(Debug, Default, Parse)]
#[grammar($spread:("...")? $value:AssignmentExpr)]
pub struct Argument{
    pub spread: Option<Span>,
    pub value: AssignmentExpr
}

#[derive(Debug, Parse)]
pub enum OptionalExpr{
    #[grammar($expr:MemberExpression $chains:OptChain*)]
    Member{
        expr: MemberExpression,
        chains: Vec<OptChain>
    },
    #[grammar($expr:CallExpr $chains:OptChain*)]
    Call{
        expr: CallExpr,
        chains: Vec<OptChain>
    },
}

#[derive(Debug, Parse)]
#[grammar($chain:OptMemberOrCall* $after:MemberOrCall)]
pub struct OptChain{
    pub chain: Vec<OptMemberOrCall>,
    pub after: Option<MemberOrCall>
}

#[derive(Debug, Parse)]
pub enum OptMemberOrCall{
    /// ?.(args)
    #[grammar("?." $args:Arguments)]
    Call{
        args: Arguments
    },
    /// ?.[prop]
    #[grammar("?." '[' $prop:Expression ']')]
    ComputedProp{
        prop: Expression
    },
    /// ?.prop
    #[grammar("?." $prop:IdentifierName)]
    Prop{
        prop: IdentifierName
    },
    /// ?.`tpl`
    #[grammar("?." $tpl: TemplateLiteral)]
    Template{
        tpl: TemplateLiteral
    },
    /// ?.#prop
    #[grammar("?." '#' $prop:IdentifierName)]
    PrivateProp{
        prop: IdentifierName
    },
}

#[derive(Debug, Parse)]
pub enum MemberOrCall{
    /// (args)
    #[grammar($args:Arguments)]
    Call{
        args: Arguments
    },
    /// [prop]
    #[grammar('[' $prop:Expression ']')]
    ComputedProp{
        prop: Expression
    },
    /// .prop
    #[grammar('.' $prop:IdentifierName)]
    Prop{
        prop: IdentifierName
    },
    /// `tpl`
    #[grammar($tpl: TemplateLiteral)]
    Template{
        tpl: TemplateLiteral
    },
    /// .#prop
    #[grammar('.' '#' $prop:IdentifierName)]
    PrivateProp{
        prop: IdentifierName
    },
}

#[derive(Debug, Parse)]
pub enum LeftHandSideExpr{
    #[grammar($item0:NewExpression)]
    New(NewExpression),
    #[grammar($item0:CallExpr)]
    Call(CallExpr),
    #[grammar($item0:OptionalExpr)]
    Optional(OptionalExpr),
}

#[derive(Debug, Parse)]
pub enum UpdateExpr{
    #[grammar($item0: LeftHandSideExpr "++")]
    SuffixIncrement(LeftHandSideExpr),
    #[grammar($item0: LeftHandSideExpr "--")]
    SuffixDecrement(LeftHandSideExpr),
    #[grammar($item0: LeftHandSideExpr)]
    Expr(LeftHandSideExpr),
    #[grammar("++" $item0: Recursive<UnaryExpr>)]
    PrefixIncrement(Box<UnaryExpr>),
    #[grammar("--" $item0: Recursive<UnaryExpr>)]
    PrefixDecrement(Box<UnaryExpr>),
}

#[derive(Debug, Parse)]
pub enum UnaryExpr{
    #[grammar("delete" $item0: UpdateExpr)]
    Delete(UpdateExpr),
    #[grammar("void" $item0: UpdateExpr)]
    Void(UpdateExpr),
    #[grammar("typeof" $item0: Recursive<UnaryExpr>)]
    TypeOf(Box<UnaryExpr>),
    #[grammar("+" $item0:Recursive<UnaryExpr>)]
    Pos(Box<UnaryExpr>),
    #[grammar("-" $item0:Recursive<UnaryExpr>)]
    Neg(Box<UnaryExpr>),
    #[grammar("~" $item0:Recursive<UnaryExpr>)]
    BitNot(Box<UnaryExpr>),
    #[grammar("!" $item0:Recursive<UnaryExpr>)]
    LogicalNot(Box<UnaryExpr>),
    #[grammar("await" $item0:Recursive<UnaryExpr>)]
    Await(Box<UnaryExpr>),
    #[grammar($item0: UpdateExpr)]
    Update(UpdateExpr),
}

#[derive(Debug, Parse)]
pub enum ExponentialExpr{
    #[grammar($item0: UnaryExpr)]
    Unary(UnaryExpr),
    #[grammar($left: UpdateExpr "**" $right: Recursive<ExponentialExpr>)]
    Expo{
        left: UpdateExpr,
        right: Box<ExponentialExpr>
    },
}

#[derive(Debug, Parse)]
pub enum MultiplicativeExpr{
    Expo(ExponentialExpr),
    #[grammar($left: Recursive<MultiplicativeExpr> "*" $right: ExponentialExpr)]
    Mul{
        left: Box<MultiplicativeExpr>,
        right: ExponentialExpr,
    }
}

#[derive(Debug, Default, Parse)]
pub enum Expression{
    D
}

#[derive(Debug)]
pub enum BindingPattern{

}