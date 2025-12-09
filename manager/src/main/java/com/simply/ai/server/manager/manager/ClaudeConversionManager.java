package com.simply.ai.server.manager.manager;

import com.simply.ai.server.manager.model.request.ClaudeRequest;
import reactor.core.publisher.Flux;

public interface ClaudeConversionManager {
    Flux<Object> streamChat(ClaudeRequest request);
}