package com.simply.ai.server.manager.manager;

import com.simply.ai.server.manager.model.request.ChatgptRequest;
import com.simply.ai.server.manager.model.response.ChatgptResponse;
import reactor.core.publisher.Flux;

public interface ChatgptConversionManager {
    Flux<ChatgptResponse> streamChat(ChatgptRequest request);
}