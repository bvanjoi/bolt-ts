import path from 'node:path'
import * as vscode from 'vscode'
import {
	type Executable,
	LanguageClient,
	type LanguageClientOptions,
	type ServerOptions
} from 'vscode-languageclient/node'

interface ClientOptions {
	logger: vscode.OutputChannel
}
interface Client {
	start(): Promise<void>
	stop(): Promise<void>
}

function createClient(options: ClientOptions): Client {
	const defaultCommand = path.resolve(
		__dirname,
		'../../../target/debug/bolt_ts_language_server'
	)
	const run: Executable = {
		command: defaultCommand
	}
	const server: ServerOptions = {
		run,
		debug: run
	}
	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ language: 'typescript' }],
		outputChannel: options.logger
	}
	const client = new LanguageClient(
		'bolt-ts-language-server',
		server,
		clientOptions
	)
	return {
		start() {
			return client.start()
		},
		stop() {
			return client.stop()
		}
	}
}

let client: Client | undefined

export function activate() {
	const logger = vscode.window.createOutputChannel('bolt-ts Extension', {
		log: true
	})

	client = createClient({ logger })

	return client.start()
}

export function deactivate() {
	if (client) {
		return client.stop()
	}
}
