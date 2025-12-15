use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types;
use tower_lsp::{Client, LanguageServer};

#[derive(Debug)]
pub struct Backend {
    client: Client,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self { client }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(
        &self,
        _: lsp_types::InitializeParams,
    ) -> Result<lsp_types::InitializeResult> {
        let capabilities = lsp_types::ServerCapabilities {
            definition_provider: Some(lsp_types::OneOf::Left(true)),
            ..Default::default()
        };
        Ok(lsp_types::InitializeResult {
            capabilities,
            ..Default::default()
        })
    }

    async fn initialized(&self, _: lsp_types::InitializedParams) {
        self.client
            .log_message(lsp_types::MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn goto_declaration(
        &self,
        params: lsp_types::request::GotoDeclarationParams,
    ) -> Result<Option<lsp_types::request::GotoDeclarationResponse>> {
        return Ok(None);
    }

    async fn goto_definition(
        &self,
        params: lsp_types::GotoDefinitionParams,
    ) -> Result<Option<lsp_types::GotoDefinitionResponse>> {
        return Ok(None);
    }
}
