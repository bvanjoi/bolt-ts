use bolt_ts_compiler::eval_from_memory_path_worker;
use bolt_ts_fs::{CachedFileSystem, LocalFS, MemoryFS};
use bolt_ts_span::ModuleID;

pub trait LanguageServiceHost<FS: CachedFileSystem> {
    fn new(fs: FS) -> Self;
    fn steal_fs(&mut self) -> FS;
}

pub struct LanguageService<'cx, FS: CachedFileSystem, Host: LanguageServiceHost<FS>> {
    host: Host,
    compiler_result: bolt_ts_compiler::CompilerResult<'cx, FS>,
}

impl<'cx, Host: LanguageServiceHost<MemoryFS>> LanguageService<'cx, MemoryFS, Host> {
    pub fn new(
        mut host: Host,
        atoms: bolt_ts_atom::AtomIntern,
        parser_arena: &'cx bolt_ts_arena::bumpalo_herd::Herd,
        type_arena: &'cx bolt_ts_arena::bumpalo::Bump,
        default_lib_dir: String,
    ) -> Self {
        let fs = host.steal_fs();
        let compiler_result = eval_from_memory_path_worker(
            "/".to_string(),
            default_lib_dir,
            parser_arena,
            type_arena,
            fs,
            atoms,
        );
        LanguageService {
            host,
            compiler_result,
        }
    }
}

impl<'cx, Host: LanguageServiceHost<LocalFS>> LanguageService<'cx, LocalFS, Host> {
    pub fn new(_: Host) -> Self {
        todo!()
    }
}

impl<'cx, FS: CachedFileSystem, Host: LanguageServiceHost<FS>> LanguageService<'cx, FS, Host> {
    pub fn get_implementation_at_position(
        &mut self,
        file_name: ModuleID,
        position: usize,
    ) -> Vec<bolt_ts_ast::Node<'cx>> {
        // TODO: synchronize_host_data
        self.compiler_result
            .get_implementation_at_position(file_name, position)
    }

    pub fn compiler_result(&self) -> &bolt_ts_compiler::CompilerResult<'cx, FS> {
        &self.compiler_result
    }
}
