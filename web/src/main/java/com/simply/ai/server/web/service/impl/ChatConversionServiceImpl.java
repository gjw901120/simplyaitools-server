package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.manager.ChatgptConversionManager;
import com.simply.ai.server.manager.manager.ClaudeConversionManager;
import com.simply.ai.server.manager.manager.DeepseekConversionManager;
import com.simply.ai.server.manager.model.request.ChatgptRequest;
import com.simply.ai.server.manager.model.request.ClaudeRequest;
import com.simply.ai.server.manager.model.request.DeepseekRequest;
import com.simply.ai.server.manager.model.response.ChatgptResponse;
import com.simply.ai.server.manager.model.response.ClaudeResponse;
import com.simply.ai.server.manager.model.response.DeepseekResponse;
import com.simply.ai.server.web.model.dto.request.chat.ChatgptConversionDTO;
import com.simply.ai.server.web.model.dto.request.chat.ClaudeConversionDTO;
import com.simply.ai.server.web.model.dto.request.chat.DeepseekConversionDTO;
import com.simply.ai.server.web.service.ChatConversionService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Sinks;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class ChatConversionServiceImpl implements ChatConversionService {

    @Autowired
    private final DeepseekConversionManager chatConversionManager;

    @Autowired
    private final ChatgptConversionManager chatgptConversionManager;

    @Autowired
    private final ClaudeConversionManager claudeConversionManager;

    @Override
    public Flux<DeepseekResponse> processDeepseekStream(DeepseekConversionDTO request) {
        // 构建请求对象
        DeepseekRequest deepseekRequest = new DeepseekRequest();
        deepseekRequest.setModel(request.getModel());
        //TODO 如何存在历史信息，查询出来拼接
        if(request.getConversionId() != null) {
            List<DeepseekRequest.Message> messages = new ArrayList<>();
            messages.add(new DeepseekRequest.Message("user", request.getPrompt()));
            deepseekRequest.setMessages(messages);
        } else {
            deepseekRequest.setMessages(List.of(new DeepseekRequest.Message("user", request.getPrompt())));
        }

        DeepseekRequest.ThinkingConfig thinkingConfig = new DeepseekRequest.ThinkingConfig();
        thinkingConfig.setType(request.getEnableDeep() ? "enabled" : "disabled");
        thinkingConfig.setType(request.getEnableWebSearch() ? "enabled" : "disabled");
        deepseekRequest.setThinking(thinkingConfig);

        // 返回响应流
        return chatConversionManager.streamChat(deepseekRequest)
                .map(response -> {
                    // 在这里可以对response进行业务处理
                    log.debug("收到DeepSeek响应块，是否最终: {}", response.getIsFinal());
                    return response;
                })
                .timeout(Duration.ofSeconds(60)) // 设置超时
                .doOnError(error -> log.error("DeepSeek流式处理失败", error));
    }

    @Override
    public Flux<ChatgptResponse> processChatgptStream(ChatgptConversionDTO request) {
        // 构建ChatgptRequest对象
        ChatgptRequest chatgptRequest = buildChatgptRequest(request);

        // 调用ChatgptConversionManager
        return chatgptConversionManager.streamChat(chatgptRequest)
                .timeout(Duration.ofSeconds(60))
                .doOnError(error -> log.error("ChatGPT流式处理失败", error));
    }

    private ChatgptRequest buildChatgptRequest(ChatgptConversionDTO dto) {
        // 根据ChatgptConversionDTO构建ChatgptRequest
        // 注意：ChatgptRequest之前已经生成，需要根据DTO填充
        // 这里假设ChatgptRequest有对应的setter方法和构造方式
        ChatgptRequest request = new ChatgptRequest();
        request.setModel(dto.getModel());
        // 构建input，根据情况添加文本和图片等
        // 注意：ChatgptConversionDTO中可能只有prompt字符串，而ChatgptRequest的input是数组
        // 我们需要将prompt转换为ChatgptRequest.InputMessage
        // 这里假设只有一个用户消息
        ChatgptRequest.InputMessage message = new ChatgptRequest.InputMessage();
        message.setRole("user");
        // 构建content，可能包含文本和图片
        List<ChatgptRequest.ContentItem> content = new ArrayList<>();
        ChatgptRequest.ContentItem textContent = new ChatgptRequest.ContentItem();
        textContent.setType("text");
        textContent.setText(dto.getPrompt());
        content.add(textContent);
        message.setContent(content);
        request.setInput(List.of(message));

        // 设置其他参数
        request.setStream(true);
        // 如果有工具调用，设置tools
        // 如果有推理配置，设置reasoning
        // 根据DTO中的enableWebSearch和enableDeep等设置
        // 注意：ChatgptRequest中的tools和reasoning需要根据实际情况设置
        if (dto.getEnableWebSearch() != null && dto.getEnableWebSearch()) {
            ChatgptRequest.Tool tool = new ChatgptRequest.Tool();
            tool.setType("web_search_preview");
            request.setTools(List.of(tool));
        }
        if (dto.getEnableDeep() != null && dto.getEnableDeep()) {
            ChatgptRequest.Reasoning reasoning = new ChatgptRequest.Reasoning();
            reasoning.setEffort("high");
            request.setReasoning(reasoning);
        }

        return request;
    }

    // ========== Claude流式方法 ==========

    @Override
    public Flux<ClaudeResponse.AggregatedEvent> processClaudeStream(ClaudeConversionDTO request) {
        // 强制设置为流式
        request.setStream(true);

        // 构建Claude请求
        ClaudeRequest claudeRequest = buildClaudeRequest(request);

        final Sinks.Many<ClaudeResponse.AggregatedEvent> sink = Sinks.many().replay().latest();
        final StringBuilder contentAccumulator = new StringBuilder();
        final ClaudeResponse.Usage[] finalUsage = {null};
        final String[] stopReason = {null};

        // 调用Manager获取原始事件流
        claudeConversionManager.streamChat(claudeRequest)
                .timeout(Duration.ofSeconds(60))
                .subscribe(
                        event -> {
                            // 处理内容增量事件
                            if (event instanceof ClaudeResponse.ContentBlockDelta) {
                                ClaudeResponse.ContentBlockDelta deltaEvent = (ClaudeResponse.ContentBlockDelta) event;
                                if (deltaEvent.getDelta() != null && deltaEvent.getDelta().getText() != null) {
                                    String deltaText = deltaEvent.getDelta().getText();
                                    contentAccumulator.append(deltaText);

                                    // 创建聚合事件
                                    ClaudeResponse.AggregatedEvent aggregatedEvent = new ClaudeResponse.AggregatedEvent();
                                    aggregatedEvent.setContent(contentAccumulator.toString());
                                    aggregatedEvent.setDelta(deltaText);
                                    aggregatedEvent.setIsFinal(false);
                                    sink.tryEmitNext(aggregatedEvent);
                                }
                            }

                            // 处理消息增量事件（包含使用情况和停止原因）
                            else if (event instanceof ClaudeResponse.MessageDelta) {
                                ClaudeResponse.MessageDelta messageDelta = (ClaudeResponse.MessageDelta) event;
                                finalUsage[0] = messageDelta.getUsage();
                                if (messageDelta.getDelta() != null) {
                                    stopReason[0] = messageDelta.getDelta().getStopReason();
                                }
                            }

                            // 处理消息停止事件
                            else if (event instanceof ClaudeResponse.MessageStop) {
                                // 发送最终聚合事件
                                ClaudeResponse.AggregatedEvent finalEvent = new ClaudeResponse.AggregatedEvent();
                                finalEvent.setContent(contentAccumulator.toString());
                                finalEvent.setIsFinal(true);
                                finalEvent.setUsage(finalUsage[0]);
                                finalEvent.setStopReason(stopReason[0]);
                                sink.tryEmitNext(finalEvent);
                                sink.tryEmitComplete();
                            }

                            // 忽略其他事件（如ping、content_block_start等）
                        },
                        error -> {
                            log.error("Claude流式处理失败", error);
                            sink.tryEmitError(error);
                        }
                );

        return sink.asFlux();
    }

    /**
     * 构建Claude请求对象
     */
    private ClaudeRequest buildClaudeRequest(ClaudeConversionDTO dto) {
        ClaudeRequest request = new ClaudeRequest();
        request.setModel(dto.getModel());
        request.setMax_tokens(dto.getMaxTokens());
        request.setStream(dto.getStream());

        // 构建消息
        ClaudeRequest.Message message = new ClaudeRequest.Message();
        message.setRole("user");

        String prompt = dto.getPrompt();

        // 处理多模态内容
        if (dto.getFiles() != null && !dto.getFiles().isEmpty()) {
            // 构建包含图片的内容数组,处理文件上传，添加到prompt

            message.setContent(prompt);
        }

        request.setMessages(List.of(message));

        // 如果启用联网搜索，添加工具
        if (Boolean.TRUE.equals(dto.getEnableWebSearch())) {
            ClaudeRequest.Tool webSearchTool = new ClaudeRequest.Tool();
            webSearchTool.setType("web_search_20250305");
            webSearchTool.setName("web_search");
            webSearchTool.setMax_uses(10);
            request.setTools(List.of(webSearchTool));
        }

        return request;
    }


}